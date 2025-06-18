#:include "num_setup.fypp"
#include "fabm_driver.h"

module imr_norwecom_num

    use fabm_types
    use globals, only: dp, rhoCN
    use NUMmodel
    use copepods, only: passive, active

    implicit none
    private

    ! NUM variables
    real(dp), allocatable :: u(:)
    real(dp), allocatable :: dudt(:)
    character(len=20) :: errorstr
    logical(1) :: errorio = .false.

    ! FABM variables
    type, extends(type_base_model), public :: type_imr_norwecom_num

        ! Declare size-structured state variables
        #:if GENERALISTS == True
            #:for i in range(NB_GENERALISTS)
                type(type_state_variable_id) :: id_gen${i+1}$ !! Generalists ${i+1}$ [ugC l-1]
            #:endfor
        #:endif
        #:if POM == True
            #:for i in range(NB_POM)
                type(type_state_variable_id) :: id_pom${i+1}$ !! Particulate organic carbon ${i+1}$ [ugC l-1]
            #:endfor
        #:endif
        #:if DIATOMS == True
            #:for i in range(NB_DIATOMS)
                type(type_state_variable_id) :: id_diatoms${i+1}$ !! Diatoms ${i+1}$ [ugC l-1]
            #:endfor
        #:endif
        #:if COPEPOD_PASSIVE == True
            #:for i in range(NB_COPPAS)
                #:for j in range(NB_COPST)
                    type(type_state_variable_id) :: id_coppasst${j+1}$si${i+1}$ !! Passive copepods stage j size i [ugC l-1]
                #:endfor    
            #:endfor
        #:endif
        #:if COPEPOD_ACTIVE == True
            #:for i in range(NB_COPACT)
                #:for j in range(NB_COPST)
                    type(type_state_variable_id) :: id_copactst${j+1}$si${i+1}$ !! Active copepods stage j size i [ugC l-1]
                #:endfor
            #:endfor
        #:endif

        ! Declare other state variables
        type(type_state_variable_id) :: id_no3 !! Nitrate [ugN l-1]
        type(type_state_variable_id) :: id_doc !! Dissolved inorganic carbon [ugC l-1]
        type(type_state_variable_id) :: id_sil !! Silicate [mgSi l-1]
        ! type(type_state_variable_id) :: id_pho !! Phosphate [mgP l-1]

        ! Declare dependencies
        type(type_dependency_id) :: id_temp !! Temperature [degC]
        type(type_dependency_id) :: id_par !! Photoactive radition

        ! Declare diagnostic variables
        type(type_diagnostic_variable_id) :: id_chla_total !! Chlorophyll a concentration [ugChla l-1]
        type(type_diagnostic_variable_id) :: id_gen_total !! Generalists concentration [ugC l-1]
        type(type_diagnostic_variable_id) :: id_pom_total !! Particulate organic matter concentration [ugC l-1]
        type(type_diagnostic_variable_id) :: id_diatom_total !! diatom concentration [ugC l-1]
        type(type_diagnostic_variable_id) :: id_coppas_total !! Passive copepod concentration [ugC l-1]
        type(type_diagnostic_variable_id) :: id_copact_total !! Active copepod concentration [ugC l-1]
        
    contains
        procedure :: initialize
        procedure :: do
        procedure :: get_vertical_movement
    end type type_imr_norwecom_num

contains

    subroutine initialize(self, configunit)
        class(type_imr_norwecom_num), intent(inout), target :: self
        integer, intent(in) :: configunit
        !real(dp) :: mAdultPassive(:), mAdultActive(:) 

        ! Initialize size-structured state variables
        ! write(*,*)"BEFORE_INITIALIZE_GENERALISTS"
        #:if GENERALISTS == True
            #:for i in range(NB_GENERALISTS)
                call self%register_state_variable(self%id_gen${i+1}$, &
                    "gen${i+1}$", "ugC l-1", "Generalists size group ${i+1}$", &
                    minimum = 0.01_rk, initial_value = 0.01_rk)
            #:endfor
        #:endif

        #:if POM == True
            #:for i in range(NB_POM)
                call self%register_state_variable(self%id_pom${i+1}$, &
                    "pom${i+1}$", "ugC l-1", "Particulate organic matter size group ${i+1}$", &
                    minimum = 0.0_rk, initial_value = 0.0_rk)
            #:endfor
        #:endif

        #:if DIATOMS == True
            #:for i in range(NB_DIATOMS)
                call self%register_state_variable(self%id_diatoms${i+1}$, &
                    "diatom${i+1}$", "ugC l-1", "Diatom size group ${i+1}$", &
                    minimum = 0.001_rk, initial_value = 0.001_rk)
            #:endfor
        #:endif

        #:if COPEPOD_PASSIVE == True
            #:for i in range(NB_COPPAS)
                #:for j in range(NB_COPST)
                call self%register_state_variable(self%id_coppasst${j+1}$si${i+1}$, &
                    "coppasst${j+1}$si${i+1}$", "ugC l-1", "Passive copepods size group ${i+1}$, stage ${j+1}$", &
                    minimum = 0.0001_rk, initial_value = 0.0001_rk)
                #:endfor    
            #:endfor
        #:endif

        #:if COPEPOD_ACTIVE == True
            #:for i in range(NB_COPACT)
                #:for j in range(NB_COPST)
                    call self%register_state_variable(self%id_copactst${j+1}$si${i+1}$, &
                        "copactst${j+1}$si${i+1}$", "ugC l-1", "Active copepod size group ${i+1}$, stage ${j+1}$", &
                        minimum = 0.0001_rk, initial_value = 0.0001_rk)
                #:endfor
            #:endfor
        #:endif

        ! Initialize other state variables
        call self%register_state_variable(self%id_no3, &
            "no3", "ugN l-1", "Nitrate concentration", &
            minimum = 0.0_rk, initial_value = 15.0_rk)
        call self%register_state_variable(self%id_doc, &
            "doc", "ugC l-1", "Dissolved inorganic carbon concentration", &
            minimum = 0.0_rk, initial_value = 150.0_rk)
        call self%register_state_variable(self%id_sil, &
            "si", "ugSi l-1", "Dissolved inorganic silicate", &
            minimum = 0.0_rk, initial_value = 1.0_rk)
        !call self%register_state_variable(self%id_pho, &
        !    "po4", "ugP l-1", "Phosphate concentration", &
        !    minimum = 0.0_rk, initial_value = 4.0_rk)

        ! Initialize dependencies
        call self%register_dependency(self%id_temp, standard_variables%temperature)
        call self%register_dependency(self%id_par, standard_variables%downwelling_photosynthetic_radiative_flux)

        ! Initialize diagnostic variables
        call self%register_diagnostic_variable(self%id_chla_total, "chla_total", "ugChla l-1", "Chlorophyll a")
        call self%register_diagnostic_variable(self%id_gen_total, "gen_total", "ugC l-1", "Generalists")
        call self%register_diagnostic_variable(self%id_pom_total, "pom_total", "ugC l-1", "POM")
        call self%register_diagnostic_variable(self%id_diatom_total,"dia_total", "ugC l-1", "DIATOMS")
        call self%register_diagnostic_variable(self%id_coppas_total,"coppas_total", "ugC l-1", "COPPAS")
        call self%register_diagnostic_variable(self%id_copact_total,"copact_total","ugC l-1", "COPACT")
        ! Initialize NUM model
        call initialize_num_model()
        allocate(u(nGrid))
        allocate(dudt(nGrid))
        
    end subroutine initialize

    subroutine initialize_num_model()
        ! A generic setup with generalists, diatoms and copepods
        !call setupGenDiatCope(${NB_GENERALISTS}$,${NB_COPST}$,${NB_POM}$,${NB_COPACT_AS}$*1.0_dp, errorio, errorstr)
        ! Full NUM model setup with generalists, copepods, and POM
        call setupNUMmodel(${NB_GENERALISTS}$,${NB_DIATOMS}$,${NB_COPST}$,${NB_COPPAS_AS}$*1.0_dp,${NB_COPACT_AS}$*1.0_dp, errorio, errorstr)
          !!!subroutine setupNUMmodel(n, nDiatoms, nCopepod, nPOM, mAdultPassive, mAdultActive,errorio,errorstr)
        
        if (errorio .eqv. .true.) then
            print *, "Error reading parameter ", errorstr
            stop
        end if
    end subroutine initialize_num_model

   subroutine do(self, _ARGUMENTS_DO_)
        class(type_imr_norwecom_num), intent(in) :: self
        _DECLARE_ARGUMENTS_DO_

        ! Local variables
        real(rk) :: par, temp, gen_total, pom_total, diatom_total, coppas_total, copact_total 
        integer :: iGroup, k
        logical :: bDonePassive, bDoneActive

        _LOOP_BEGIN_

        ! Reset arrays
        u = 0.0
        dudt = 0.0

        ! Get local copy of state variables

        ! First nutrient and generalists as they use their own indexing
        _GET_(self%id_no3, u(idxN))
        _GET_(self%id_sil, u(idxSi))
        !_GET_(self%id_pho, u(idxP))
        _GET_(self%id_doc, u(idxDOC))
        
        ! Then we get the other groups (using ixStart)
        bDonePassive = .false.
        bDoneActive = .false.
        do iGroup = 1, nGroups
            select type(spec => group(iGroup)%spec)

            type is(spectrumGeneralists)
                    #:for i in range(NB_GENERALISTS)
                        _GET_(self%id_gen${i+1}$, u(idxB+${i}$))
                    #:endfor
            
            #:if POM == True
            type is(spectrumPOM)
                    #:for i in range(NB_POM)
                        _GET_(self%id_pom${i+1}$, u(ixStart(iGroup)+${i}$))
                    #:endfor
            #:endif

            #:if DIATOMS == True
            type is(spectrumDIATOMS)
                    #:for i in range(NB_DIATOMS)
                        _GET_(self%id_diatoms${i+1}$, u(ixStart(iGroup)+${i}$))
                    #:endfor
            #:endif
 
            type is(spectrumCOPEPOD)
                k = 0
                #:if COPEPOD_PASSIVE == True
                if (spec%feedingmode .eq. passive) then
                    if (.not. bDonePassive) then
                    #:for i in range(NB_COPPAS)
                        #:for j in range(NB_COPST)
                            _GET_(self%id_coppasst${j+1}$si${i+1}$, u(ixStart(iGroup)+k))
                            k=k+1
                        #:endfor  
                    #:endfor  
                    bDonePassive = .true.
                    end if
                end if
                #:endif

                #:if COPEPOD_ACTIVE == True
                if (spec%feedingmode .eq. active) then 
                    if (.not. bDoneActive) then
                        #:for i in range(NB_COPACT)
                            #:for j in range(NB_COPST)
                                _GET_(self%id_copactst${j+1}$si${i+1}$, u(ixStart(iGroup)+k))
                                k=k+1
                            #:endfor    
                        #:endfor
                        bDoneActive = .true.
                    end if
                end if
                #:endif
            end select
        end do

        ! Lastly we get the environmental dependencies
        _GET_(self%id_temp, temp)
        _GET_(self%id_par, par)

        ! Convert photosynthetic radiative flux
        par = par / 0.217 ! W m-2 -> umol m-2 s-1

        ! Perform daystep in NUM
 

        call calcDerivatives(u, par, temp, 1.0_dp, dudt)
        _SET_DIAGNOSTIC_(self%id_chla_total, calc_chla(par))

         !open(unit=10, file='NUMmodel.txt', status='old', action='write')
 !do iGroup = 1 , nGroups
 !     write(*,*) 'Group ', iGroup, ': ', iGroup
 !     write(*,*) '  n = ', group(iGroup)%spec%n
 !     write(*,*) '  ixStart = ', ixStart(iGroup)
 !     write(*,*) '  u = ', upositive( ixStart(iGroup):ixEnd(iGroup) )
 !     write(*,*) '  dudt = ', dudt(ixStart(iGroup):ixEnd(iGroup) )
 !   end do
  !  close(10)



        ! Update rates in FABM
        coppas_total = 0.0
        copact_total = 0.0
        bDoneActive = .false.
        bDonePassive = .false.
        do iGroup = 1, nGroups
            select type(spec => group(iGroup)%spec)

            type is(spectrumGeneralists)
                    gen_total = 0.0
                    #:for i in range(NB_GENERALISTS)
                        gen_total = gen_total + real(u(idxB+${i}$), kind=rk)
                        _ADD_SOURCE_(self%id_gen${i+1}$, real(dudt(idxB+${i}$), kind=rk)/86400.0)
                    #:endfor
                    _SET_DIAGNOSTIC_(self%id_gen_total, gen_total)

            #:if POM == True
            type is(spectrumPOM)
                    pom_total = 0.0
                    #:for i in range(NB_POM)
                        pom_total = pom_total + real(u(ixStart(iGroup)+${i}$), kind=rk)
                        _ADD_SOURCE_(self%id_pom${i+1}$, real(dudt(ixStart(iGroup)+${i}$), kind=rk)/86400.0)
                    #:endfor
                    _SET_DIAGNOSTIC_(self%id_pom_total, pom_total)
            #:endif

            #:if DIATOMS == True
            type is(spectrumDIATOMS)
                    diatom_total = 0.0
                    #:for i in range(NB_DIATOMS)
                        diatom_total = diatom_total + real(u(ixStart(iGroup)+${i}$), kind=rk)
                        _ADD_SOURCE_(self%id_diatoms${i+1}$, real(dudt(ixStart(iGroup)+${i}$), kind=rk)/86400.0)
                    #:endfor
                    _SET_DIAGNOSTIC_(self%id_diatom_total, diatom_total)
            #:endif
            
            type is(spectrumCOPEPOD)
                k = 0
                #:if COPEPOD_PASSIVE == True
                if (spec%feedingmode == passive) then
                    if (.not. bDonePassive) then
                    #:for i in range(NB_COPPAS)
                        #:for j in range(NB_COPST)
                            coppas_total = coppas_total + real(u(ixStart(iGroup)+k), kind=rk)
                            _ADD_SOURCE_(self%id_coppasst${j+1}$si${i+1}$, real(dudt(ixStart(iGroup)+k), kind=rk)/86400.0)
                            k=k+1
                        #:endfor  
                    #:endfor  
                    bDonePassive = .true.
                    end if
                end if
                #:endif

                #:if COPEPOD_ACTIVE == True
                if (spec%feedingmode == active) then
                    if (.not. bDoneActive) then
                        #:for i in range(NB_COPACT)
                            #:for j in range(NB_COPST)
                                copact_total = copact_total + real(u(ixStart(iGroup)+k), kind=rk)
                                _ADD_SOURCE_(self%id_copactst${j+1}$si${i+1}$,real(dudt(ixStart(iGroup)+k), kind=rk)/86400.0)
                                k=k+1
                            #:endfor    
                        #:endfor
                        bDoneActive = .true.
                    end if
                end if
                #:endif

            end select
        end do

        #:if COPEPOD_PASSIVE == True
            _SET_DIAGNOSTIC_(self%id_coppas_total, coppas_total)
        #:endif

        #:if COPEPOD_ACTIVE == True
            _SET_DIAGNOSTIC_(self%id_copact_total, copact_total)
        #:endif

        _ADD_SOURCE_(self%id_no3, real(dudt(idxN), kind=rk)/86400.0)
        _ADD_SOURCE_(self%id_doc, real(dudt(idxDOC), kind=rk)/86400.0)
        _ADD_SOURCE_(self%id_sil, real(dudt(idxSi), kind=rk)/86400.0) 

        _LOOP_END_
    end subroutine do

    subroutine get_vertical_movement(self, _ARGUMENTS_GET_VERTICAL_MOVEMENT_)
        class(type_imr_norwecom_num), intent(in) :: self
        _DECLARE_ARGUMENTS_GET_VERTICAL_MOVEMENT_

        ! Local variables
        integer :: iGroup
        real(rk) :: r, vpom

        _LOOP_BEGIN_

        do iGroup = 1, nGroups
            select type(spec => group(iGroup)%spec)
            type is(spectrumPOM)
                #:if POM == True
                    #:for i in range(NB_POM)
                        ! Sinking according to Stoke's Law (See Kiørboe 1993)
                        r = real(((3.0/(4.0*pi)*spec%m(${i+1}$)/0.4d-6)**onethird), kind=rk)
                        vpom = 1091.0*(r/1000000*10)**2
                        _SET_VERTICAL_MOVEMENT_(self%id_pom${i+1}$, -vpom)
                    #:endfor
                #:endif
            end select
        end do

        _LOOP_END_
    end subroutine get_vertical_movement

    function calc_chla(l) result(chla)
        real(rk), intent(in) :: l !! Light
        real(rk) :: chla !! Chlorophyll concentration [ugChla l-1]

        ! Local variables
        integer :: iGroup
        chla = 0.0
        
        do iGroup = 1, nGroups
            select type(spec => group(iGroup)%spec)
            type is(spectrumGeneralists)
                #:if GENERALISTS == True
                    #:for i in range(NB_GENERALISTS)
                        chla = chla + max(0.0, (real(u(idxB+${i}$)*spec%JLreal(${i+1}$), kind=rk))/l)
                    #:endfor
                #:endif
            end select
        end do
    end function calc_chla

end module imr_norwecom_num
