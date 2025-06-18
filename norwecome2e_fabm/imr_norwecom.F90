#include "fabm_driver.h"

module imr_norwecom

    use fabm_types

    implicit none

    private

    ! Global parameters
    real(rk), parameter :: eps = 1.0e-12_rk !! Small number to avoid dividing by zero

    type, extends(type_base_model), public :: type_imr_norwecom
    
        ! Define pelagic biogeochemical state variables
        type(type_state_variable_id) :: id_nit !! Nitrate concentration (mgN m-3)
        type(type_state_variable_id) :: id_pho !! Phosphate concentration (mgP m-3)
        type(type_state_variable_id) :: id_sil !! Silicate concentration (mgSi m-3)
        type(type_state_variable_id) :: id_sis !! Biogenic silica concentration (mgSi m-3)
        type(type_state_variable_id) :: id_det !! Nitrogen detritus concentration (mgN m-3)
        type(type_state_variable_id) :: id_detp !! Phosphorus detritus concentration (mgP m-3)
        type(type_state_variable_id) :: id_oxy !! Dissolved oxygen concentration (mgO m-3)
        type(type_state_variable_id) :: id_dia !! Diatoms concentration (mgN m-3)
        type(type_state_variable_id) :: id_fla !! Flagellates concentration (mgN m-3)
        type(type_state_variable_id) :: id_mic !! Microzooplankton concentration (mgN m-3)
        type(type_state_variable_id) :: id_mes !! Mesozooplankton concentration (mgN m-3)
        type(type_state_variable_id) :: id_dsnk !! Detritus dynamic sinking speed tracer

        ! Define bottom biogeochemical state variables
        type(type_bottom_state_variable_id) :: id_botdet !! Bottom nitrogen detritus concentration (mgN m-2)
        type(type_bottom_state_variable_id) :: id_botdetp !! Bottom phosphorus detritus concentration (mgP m-2)
        type(type_bottom_state_variable_id) :: id_botsis !! Bottom biogenic silica concentration (mgSi m-2)
        type(type_bottom_state_variable_id) :: id_burdet !! Burried nitrogen detritus concentration (mgN m-2)
        type(type_bottom_state_variable_id) :: id_burdetp !! Burried phosphorus detritus concentration (mgP m-2)
        type(type_bottom_state_variable_id) :: id_bursis !! Burried biogenic silica concentration (mgSi m-2)

        ! Define pelagic environmental dependencies
        type(type_dependency_id) :: id_temp !! Temperature (degC)
        type(type_dependency_id) :: id_salt !! Salinity (practical)
        type(type_dependency_id) :: id_dens !! Sea water density (kg m-3)
        type(type_dependency_id) :: id_par !! Photoactive radiation (W m-2)

        ! Define bottom environmental dependences
        type(type_horizontal_dependency_id) :: id_bstress !! Bottom stress (Pa)

        ! Define pelagic diagnostic variables
        type(type_diagnostic_variable_id) :: id_chla !! Chlorophyll a concentration (mgChla m-3)
        type(type_diagnostic_variable_id) :: id_gpp !! Gross primary production (mgC m-3 s-1)
        type(type_diagnostic_variable_id) :: id_npp !! Net primary production (mgC m-3 s-1)
        type(type_diagnostic_variable_id) :: id_gsp !! Gross secondary production (mgC m-3 s-1)
        type(type_diagnostic_variable_id) :: id_nsp !! Net secondary production (mgC m-3 s-1)
        type(type_diagnostic_variable_id) :: id_oxy2 !! Oxygen concentration (umol kg-1)
        type(type_diagnostic_variable_id) :: id_snkspd !! Detritus average sinking speed (m d-1)

        ! Define surface diagnostic variables
        type(type_surface_diagnostic_variable_id) :: id_doxy
        
        ! Define model parameters
        real(rk) :: cnit !! Nitrogen atomic weight (g mol-1)
        real(rk) :: cpho !! Phosphorus atomic weight (g mol-1)
        real(rk) :: csil !! Silicium atomic weight (g mol -1)
        real(rk) :: a1 !! Diatoms pmax at 0 degC (s-1)
        real(rk) :: a2 !! Diatoms pmax temperature dependence (degC-1)
        real(rk) :: a3 !! Flagellates pmax at 0 degC (s-1)
        real(rk) :: a4 !! Flagellates pmax temperature dependence (degC-1)
        real(rk) :: a5 !! Phytoplankton respiration rate at 0 degC
        real(rk) :: a6 !! Phytoplankton respiration temperature dependence (degC-1)
        real(rk) :: dia_kr !! Diatoms half-saturation constant for light harvesting (m2 uE-1)
        real(rk) :: dia_kn !! Diatoms half-saturation constant for nitrate uptake (mmolN m-3)
        real(rk) :: dia_kp !! Diatoms half-saturation constant for phosphate uptake (mmolP m-3)
        real(rk) :: dia_ks !! Diatoms half-saturation constant for silicate uptake (mmolSi m-3)
        real(rk) :: fla_kr !! Flagellates half-saturation constant for light harvesting (m2 uE-1)
        real(rk) :: fla_kn !! Flagellates half-saturation constant for nitrate uptake (mmolN m-3)
        real(rk) :: fla_kp !! Flagellates half-saturation constant for phosphate uptake (mmolP m-3)
        real(rk) :: t_ref !! Reference temperature for substrate affinity (degC)
        real(rk) :: dia_ar !! Diatoms affinity for light harvesting (m2 uE-1) ???
        real(rk) :: dia_an !! Diatoms affinity for nitrate uptake (s-1 (mgN m-3)-1)
        real(rk) :: dia_ap !! Diatoms affinity for phosphate uptake (s-1 (mgP m-3)-1)
        real(rk) :: dia_as !! Diatoms affinity for silicate uptake (s-1 (mgSi m-3)-1)
        real(rk) :: fla_ar !! Flagellates affinity for light harvesting (m2 uE-1) ???
        real(rk) :: fla_an !! Flagellates affinity for nitrate uptake (s-1 (mgN m-3)-1)
        real(rk) :: fla_ap !! Flagellates affinity for phosphate uptake (s-1 (mgP m-3)-1)
        real(rk) :: cc1 !! Intercellular P/N weight ratio (mgP mgN-1)
        real(rk) :: cc2 !! Intercellular Si/N weight ratio (mgSi mgN-1)
        real(rk) :: cc3 !! Phytoplankton mortality rate (s-1)
        real(rk) :: cc4 !! Detritus remineralization rate (s-1)
        real(rk) :: dia_min !! Minimum diatoms concentration (mgN m-3)
        real(rk) :: fla_min !! Minimum flagellates concentration (mgN m-3)
        real(rk) :: n2chla !! Intercellular N/CHLA weight ratio (mgN mgChla-1)
        real(rk) :: sib !! Silicate concentration where sinking rate of diatoms is sr_dia_max (mmolSi m-3)
        real(rk) :: sr_dia_min !! Minimum diatoms sinking speed (m s-1)
        real(rk) :: sr_dia_max !! Maximum diatoms sinking speed (m s-1)
        real(rk) :: sr_det !! Detritus sinking rate (m s-1)
        real(rk) :: sr_sis !! Biogenic silica sinking speed (m s-1)
        real(rk) :: sr_fla !! Flagellates sinking speed (m s-1)
        real(rk) :: scc1 !! O/N consumption weight ratio (mgO mgN-1)
        real(rk) :: scc2 !! Intercellular C/N weight ratio (mgC mgN-1)
        real(rk) :: scc4 !! Biogenic silica remineralization rate (s-1)
        real(rk) :: pi11 !! Mesozooplankton prey preference for diatoms (0-1)
        real(rk) :: pi12 !! Mesozooplankton prey preference for microzooplankton (0-1)
        real(rk) :: pi13 !! Mesozooplankton prey preference for detritus (0-1)
        real(rk) :: pi21 !! Microzooplankton prey preference for flagellates (0-1)
        real(rk) :: pi22 !! Microzooplankton prey preference for detritus (0-1)
        real(rk) :: k3 !! Half-saturation constant for zooplankton ingestion (mmolN m-3)
        real(rk) :: k6 !! Half-saturation constant for zooplankton loss (mmolN m-3)
        real(rk) :: q10 !! Temperature dependence on zooplankton growth
        real(rk) :: mju2 !! Maximum loss rate of zooplankton (s-1)
        real(rk) :: beta !! Zooplankton assimilation efficiency (0-1)
        real(rk) :: delta !! Fraction of zooplankton loss to detritus (0-1)
        real(rk) :: mes_g !! Mesozooplankton maximum growth rate (s-1)
        real(rk) :: mic_g !! Microzooplankton maximum growth rate (s-1)
        real(rk) :: v !! Chlorophyll a light extinction coefficient (m mgChla-1)
        real(rk) :: p_vel !! Air-water oxygen exchange scaling factor
        real(rk) :: tau1 !! Bottom stress threshold for sedimentation (Pa)
        real(rk) :: tau2 !! Bottom stress threshold for resuspension (Pa)
        real(rk) :: c2 !! Slope of the linear increase in bottom flux (s m-1)
        real(rk) :: scc8 !! Burial rate (s-1; 120 days)
        real(rk) :: det_bul !! Detritus burial lower limit (mg m-2)
        real(rk) :: det_max !! Detritus concentration corresponding to maximum nitrogen flux (mg m-2)
        real(rk) :: sis_bul !! Biogenic silica burial lower limit (mg m-2)
        real(rk) :: detp_bul !! Phosphorus detritus burial lower limit (mg m-2)
        real(rk) :: sr_dia2det !! Sinking speed of detritus comming from diatoms (m s-1)
        real(rk) :: sr_fla2det !! Sinking speed of detritus comming from flagellates (m s-1)
        real(rk) :: sr_mic2det !! Sinking speed of detritus comming from microzooplankton (m s-1)
        real(rk) :: sr_mes2det !! Sinking speed of detritus comming from mesozooplankton (m s-1)
        logical :: use_dsnk !! Use detritus dynamic sinking speed
    contains
        procedure :: initialize
        procedure :: do_surface
        procedure :: do
        procedure :: do_bottom
        procedure :: get_vertical_movement
        procedure :: get_light_extinction
    end type

contains

    subroutine initialize(self, configunit)
        !! Initializes the NORWECOM FABM model
        class(type_imr_norwecom), intent(inout), target :: self !! NORWECOM model class
        integer, intent(in) :: configunit

        ! Local variables
        real(rk) :: dia_ar, dia_an, dia_ap, dia_as, fla_ar, fla_an, fla_ap

        ! Initialize parameters
        call self%get_parameter(self%cnit, "cnit", "g mol-1", "Nitrogen atomic weight", default = 14.01_rk)
        call self%get_parameter(self%cpho, "cpho", "g mol-1", "Phosphorus atomic weight", default = 30.97_rk)
        call self%get_parameter(self%csil, "csil", "g mol-1", "Silicium atomic weight", default = 28.09_rk)
        call self%get_parameter(self%a1, "a1", "s-1", "Diatoms pmax at 0 degC", default = 1.53e-5_rk)
        call self%get_parameter(self%a2, "a2", "degC-1", "Diatoms pmax temperature dependence", default = 0.063_rk)
        call self%get_parameter(self%a3, "a3", "s-1", "Flagellates pmax at 0 degC", default = 1.02e-5_rk)
        call self%get_parameter(self%a4, "a4", "degC-1", "Flagellates pmax temperature dependence", default = 0.063_rk)
        call self%get_parameter(self%a5, "a5", "s-1", "Phytoplankton respiration rate at 0 degC", default = 8.05e-7_rk)
        call self%get_parameter(self%a6, "a6", "degC-1", "Phytoplankton respiration temperature dependence", default = 0.07_rk)
        call self%get_parameter(self%dia_kr, "dia_kr", "m2 uE-1", "Diatoms half-saturation constant for light harvesting", default = 96.0_rk)
        call self%get_parameter(self%dia_kn, "dia_kn", "mmolN m-3", "Diatoms half-saturation constant for nitrate uptake", default = 2.0_rk)
        call self%get_parameter(self%dia_kp, "dia_kp", "mmolP m-3", "Diatoms half-saturation constant for phosphate uptake", default = 0.125_rk)
        call self%get_parameter(self%dia_ks, "dia_ks", "mmolSi m-3", "Diatoms half-saturation constant for silicate uptake", default = 1.4_rk)
        call self%get_parameter(self%fla_kr, "fla_kr", "m2 uE-1", "Flagellates half-saturation constant for light harvesting",default = 209.0_rk) !209
        call self%get_parameter(self%fla_kn, "fla_kn", "mmolN m-3", "Flagellates half-saturation constant for nitrate uptake", default = 1.5_rk) !1.5
        call self%get_parameter(self%fla_kp, "fla_kp", "mmolP m-3", "Flagellates half-saturation constant for phosphate uptake", default = 0.094_rk) !0.094
        call self%get_parameter(self%t_ref, "t_ref", "degC", "Reference temperature for substrate affinity", default = 13.0_rk)
        call get_affinities(dia_ar, dia_an, dia_ap, dia_as, fla_ar, fla_an, fla_ap)
        call self%get_parameter(self%dia_ar, "dia_ar", "m2 uE-1", "Diatoms affinity for light harvesting", default = dia_ar)
        call self%get_parameter(self%dia_an, "dia_an", "s-1 (mgN m-3)-1", "Diatoms affinity for nitrate uptake", default = dia_an)
        call self%get_parameter(self%dia_ap, "dia_ap", "s-1 (mgP m-3)-1", "Diatoms affinity for phosphate uptake", default = dia_ap)
        call self%get_parameter(self%dia_as, "dia_as", "s-1 (mgSi m-3)-1", "Diatoms affinity for silicate uptake", default = dia_as)
        call self%get_parameter(self%fla_ar, "fla_ar", "m2 uE-1", "Flagellates affinity for light harvesting", default = fla_ar)
        call self%get_parameter(self%fla_an, "fla_an", "s-1 (mgN m-3)-1", "Flagellates affinity for nitrate uptake", default = fla_an)
        call self%get_parameter(self%fla_ap, "fla_ap", "s-1 (mgP m-3)-1", "Flagellates affinity for phosphate uptake", default = fla_ap)
        call self%get_parameter(self%cc1, "cc1", "mgP mgN-1", "Intercellular P/N weight ratio", default = 0.138_rk)
        call self%get_parameter(self%cc2, "cc2", "mgSi mgN-1", "Intercellular Si/N weight ratio", default = 1.75_rk)
        call self%get_parameter(self%cc3, "cc3", "s-1", "Phytoplankton mortality rate", default = 1.6e-7_rk)
        call self%get_parameter(self%cc4, "cc4", "s-1", "Detritus remineralization rate", default = 1.9e-7_rk) !1.52e-7
        call self%get_parameter(self%dia_min, "dia_min", "mgN m-3", "Minimum diatoms concentration", default = 0.1_rk)
        call self%get_parameter(self%fla_min, "fla_min", "mgN m-3", "Minimum flagellate concentration", default = 0.1_rk)
        call self%get_parameter(self%n2chla, "n2chla", "mgN mgChla-1", "Intercellular N/CHLA weight ratio", default = 11.0_rk)
        call self%get_parameter(self%sib, "sib", "mmolSi m-3", "Silicate concentration where sinking rate of diatoms is sr_dia_max", default = 1.0_rk)
        call self%get_parameter(self%sr_dia_min, "sr_dia_min", "m s-1", "Minimum diatoms sinking speed", default = 3.47e-6_rk)
        call self%get_parameter(self%sr_dia_max, "sr_dia_max", "m s-1", "Maximum diatoms sinking speed", default = 3.47e-5_rk)
        call self%get_parameter(self%sr_det, "sr_det", "m s-1", "Detritus sinking speed", default = 3e-5_rk) !3.47E-5
        call self%get_parameter(self%sr_sis, "sr_sis", "m s-1", "Biogenic silica sinking speed", default = 3e-5_rk) !3.47E-5
        call self%get_parameter(self%sr_fla, "sr_fla", "m s-1", "Flagellates sinking speed", default = 3e-6_rk) !2.89
        call self%get_parameter(self%scc1, "scc1", "mgO mgN-1", "O/N consumption weight ratio", default = 19.71_rk)
        call self%get_parameter(self%scc2, "scc2", "mgC mgN-1", "Intercellular C/N weight ratio", default = 5.68_rk)
        call self%get_parameter(self%scc4, "scc4", "s-1", "Biogenic silica remineralization rate", default = 6.41e-8_rk)
        call self%get_parameter(self%pi11, "pi11", "0-1", "Mesozooplankton prey preference for diatoms", default = 0.333_rk)
        call self%get_parameter(self%pi12, "pi12", "0-1", "Mesozooplankton prey preference for microzooplankton", default = 0.333_rk)
        call self%get_parameter(self%pi13, "pi13", "0-1", "Mesozooplankton prey preference for detritus", default = 0.333_rk)
        call self%get_parameter(self%pi21, "pi21", "0-1", "Microzooplankton prey preference for flagellates", default = 0.5_rk)
        call self%get_parameter(self%pi22, "pi22", "0-1", "Microzooplankton prey preference for detritus", default = 0.5_rk)
        call self%get_parameter(self%k3, "k3", "mmolN m-3", "Half-saturation constant for zooplankton ingestion", default = 1.0_rk)
        call self%get_parameter(self%k6, "k6", "mmolN m-3", "Half-saturation constant for zooplankton loss", 0.2_rk)
        call self%get_parameter(self%q10, "q10", "", "Temperature dependence on zooplankton growth", default = 1.5_rk)
        call self%get_parameter(self%mju2, "mju2", "s-1", "Maximum loss rate of zooplankton", default = 2.32e-6_rk)
        call self%get_parameter(self%beta, "beta", "0-1", "Zooplankton assimilation efficiency", default = 0.75_rk)
        call self%get_parameter(self%delta, "delta", "0-1", "Fraction of zooplankton loss to detritus", default = 0.6_rk) 
        call self%get_parameter(self%mes_g, "mes_g", "s-1", "Mesozooplankton maximum growth rate", default = 6e-6_rk) !4.63
        call self%get_parameter(self%mic_g, "mic_g", "s-1", "Microzooplankton maximum growth rate", default = 7e-6_rk) !5.58
        call self%get_parameter(self%v, "v", "m mgChla-1", "Chlorophyll a light extinction coefficient", default = 1.38e-2_rk)
        call self%get_parameter(self%p_vel, "p_vel", "", "Air-water oxygen exchange scaling factor", default = 1.0_rk)
        call self%get_parameter(self%tau1, "tau1", "Pa", "Bottom stress threshold for sedimentation", default = 0.064_rk)
        call self%get_parameter(self%tau2, "tau2", "Pa", "Bottom stress threshold for resuspension", default = 0.78_rk)
        call self%get_parameter(self%c2, "c2", "s m-1", "Slope of the linear increase in bottom flux", default = 100.0_rk)
        call self%get_parameter(self%scc8, "scc8", "s-1", "Burial rate", default = 9.65e-8_rk)
        call self%get_parameter(self%det_bul, "mg m-2", "Detritus burial lower limit", default = 630.0_rk)
        call self%get_parameter(self%det_max, "mg m-2", "Detritus concentration corresponding to maximum nitrogen flux", default = 1.0e5_rk)
        call self%get_parameter(self%sis_bul, "mg m-2", "Biogenic silica burial lower limit", default = 1100.0_rk)
        call self%get_parameter(self%detp_bul, "mg m-2", "Phosphorus detritus burial lower limit", default = 63.0_rk)
        call self%get_parameter(self%use_dsnk, "use_dsnk", "", "Use detritus dynamic sinking speed", default = .false.)
        call self%get_parameter(self%sr_dia2det, "sr_dia2det", "m s-1", "Sinking speed of detritus comming from diatoms", default = 0.00005787_rk)
        call self%get_parameter(self%sr_fla2det, "sr_fla2det", "m s-1", "Sinking speed of detritus comming from flagellates", default = 0.00001157_rk)
        call self%get_parameter(self%sr_mic2det, "sr_mic2det", "m s-1", "Sinking speed of detritus comming from microzooplankton", default = 0.00002315_rk)
        call self%get_parameter(self%sr_mes2det, "sr_mes2det", "m s-1", "Sinking speed of detritus comming from mesozooplankton", default = 0.00005787_rk)
        
        ! Initialize pelagic biogeochemical state variables
        call self%register_state_variable(self%id_nit, "nit", "mgN m-3", "Nitrate concentration", minimum = 0.0_rk, initial_value = 168.0_rk)
        call self%register_state_variable(self%id_pho, "pho", "mgP m-3", "Phosphate concentration", minimum = 0.0_rk, initial_value = 25.0_rk)
        call self%register_state_variable(self%id_sil, "sil", "mgSi m-3", "Silicate concentration", minimum = 0.0_rk, initial_value = 155.0_rk)
        call self%register_state_variable(self%id_sis, "sis", "mgSi m-3", "Biogenic silica concentration", minimum = 0.0_rk, initial_value = 0.1_rk)
        call self%register_state_variable(self%id_det, "det", "mgN m-3", "Nitrogen detritus concentration", minimum = 0.0_rk, initial_value = 0.1_rk)
        call self%register_state_variable(self%id_detp, "detp", "mgP m-3", "Phosphorus detritus concentration", minimum = 0.0_rk, initial_value = 0.1_rk)
        call self%register_state_variable(self%id_oxy, "oxy", "mgO m-3", "Dissolved oxygen concentration", minimum = 0.0_rk, initial_value = 10.0_rk)
        call self%register_state_variable(self%id_dia, "dia", "mgN m-3", "Diatoms concentration", minimum = 1e-4_rk, initial_value = 0.1_rk)
        call self%register_state_variable(self%id_fla, "fla", "mgN m-3", "Flagellates concentration", minimum = 1e-4_rk, initial_value = 0.1_rk)
        call self%register_state_variable(self%id_mic, "mic", "mgN m-3", "Microzooplankton concentration", minimum = 1e-4_rk, initial_value = 0.1_rk)
        call self%register_state_variable(self%id_mes, "mes", "mgN m-3", "Mesozooplankton concentration", minimum = 1e-4_rk, initial_value = 0.1_rk)
        call self%register_state_variable(self%id_dsnk, "dsnk", "", "Detritus dynamic sinking speed tracer", minimum = 0.0_rk, initial_value = 0.000003_rk)
        
        ! Initialize bottom biogeochemical variables
        call self%register_state_variable(self%id_botdet, "botdet", "mgN m-2", "Bottom nitrogen detritus concentration", minimum = 1e-4_rk, initial_value = 0.1_rk)
        call self%register_state_variable(self%id_botdetp, "botdetp", "mgP m-2", "Bottom phosphorus detritus concentration", minimum = 1e-4_rk, initial_value = 0.1_rk)
        call self%register_state_variable(self%id_botsis, "botsis", "mgSi m-2", "Bottom biogenic silica concentration", minimum = 1e-4_rk, initial_value = 0.1_rk)
        call self%register_state_variable(self%id_burdet, "burdet", "mgN m-2", "Burried nitrogen detritus concentration", minimum = 0.0_rk, initial_value = 0.0_rk)
        call self%register_state_variable(self%id_burdetp, "burdetp", "mgP m-2", "Burried phosphorus detritus concentration", minimum = 0.0_rk, initial_value = 0.0_rk)
        call self%register_state_variable(self%id_bursis, "bursis", "mgSi m-2", "Burried biogenic silica concentration", minimum = 0.0_rk, initial_value = 0.0_rk)

        ! Initialize pelagic environmental dependencies
        call self%register_dependency(self%id_temp, standard_variables%temperature)
        call self%register_dependency(self%id_salt, standard_variables%practical_salinity)
        call self%register_dependency(self%id_dens, standard_variables%density)
        call self%register_dependency(self%id_par, standard_variables%downwelling_photosynthetic_radiative_flux)
        
        ! Initialize bottom environmental dependencies
        call self%register_horizontal_dependency(self%id_bstress, standard_variables%bottom_stress)

        ! Initialize pelagic diagnostic variables
        call self%register_diagnostic_variable(self%id_chla, "chla", "mgChla m-3", "Chlorophyll a concentration")
        call self%register_diagnostic_variable(self%id_gpp, "gpp", "mgC m-3 s-1", "Gross primary production")
        call self%register_diagnostic_variable(self%id_npp, "npp", "mgC m-3 s-1", "Net primary production")
        call self%register_diagnostic_variable(self%id_gsp, "gsp", "mgC m-3 s-1", "Gross secondary production")
        call self%register_diagnostic_variable(self%id_nsp, "nsp", "mgC m-3 s-1", "Net secondary production")
        call self%register_diagnostic_variable(self%id_oxy2, "oxy2", "umol kg-1", "Oxygen concentration")
        call self%register_diagnostic_variable(self%id_snkspd, "snkspd", "m d-1", "Detritus average sinking speed")

        ! Initialize horizontal diagnostic variables
        call self%register_surface_diagnostic_variable(self%id_doxy, "doxy", "mg m-2 s-1", "Oxygen air-sea exchange")
    
        ! Initialize aggregated variables
        call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_nit)
        call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_det)
        call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_dia)
        call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_fla)
        call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_mic)
        call self%add_to_aggregate_variable(standard_variables%total_nitrogen, self%id_mes)
    contains

        subroutine get_affinities(dia_ar, dia_an, dia_ap, dia_as, fla_ar, fla_an, fla_ap)
            !! Calculates substrate affinities for diatoms and flagellates
            real(rk), intent(out) :: dia_ar, dia_an, dia_ap, dia_as !! Affinities for diatoms
            real(rk), intent(out) :: fla_ar, fla_an, fla_ap !! Affinities for flagellates
            
            ! Local variables
            real(rk) :: pmax

            ! Diatoms
            pmax = self%a1*exp(self%t_ref*self%a2)
            dia_ar = pmax/self%dia_kr
            dia_an = pmax/(self%cnit*self%dia_kn)
            dia_ap = pmax/(self%cpho*self%dia_kp)
            dia_as = pmax/(self%csil*self%dia_ks)

            ! Flagellates
            pmax = self%a3*exp(self%t_ref*self%a4)
            fla_ar = pmax/self%fla_kr
            fla_an = pmax/(self%cnit*self%fla_kn)
            fla_ap = pmax/(self%cpho*self%fla_kp)
        end subroutine get_affinities

    end subroutine initialize

    subroutine do_surface(self, _ARGUMENTS_DO_SURFACE_)
        !! Performs surface processes
        class(type_imr_norwecom), intent(in) :: self
        _DECLARE_ARGUMENTS_DO_SURFACE_

        ! Local variables
        real(rk) :: temp, salt, dens, oxy
        real(rk) :: tempk, pvel, osat, doxy_dt

        ! Local parameters
        ! Kester, D.R.,1975. Dissolved gases other than CO2. In: Riley, J.P. and G. Skirrow (eds), 
        ! Chemical Oceanography, 2.nd ed., Vol. 1, Academic Press, New York.
        real(rk), parameter :: a(4) = [-173.9894_rk, 255.5907_rk, 146.4813_rk, -22.2040_rk]
        real(rk), parameter :: b(3) = [-0.037376_rk, 0.016504_rk, -0.0020564_rk]
        real(rk), parameter :: tkel = 273.15_rk

        _SURFACE_LOOP_BEGIN_

        ! Get local copy of state variables
        _GET_(self%id_temp, temp)
        _GET_(self%id_salt, salt)
        _GET_(self%id_dens, dens)
        _GET_(self%id_oxy, oxy)

        ! Get temperature in Kelvin
        tempk = temp + tkel
        tempk = tempk/100.0_rk

        ! Calculate oxygen saturation state
        osat = exp(a(1) + a(2)/tempk + a(3)*log(tempk) + a(4)*(tempk) &
            + salt*(b(1) + b(2)*(tempk) + b(3)*tempk*tempk)) ! mol kg-1
        osat = osat*dens ! mol m-3
        osat = osat*32.0_rk*1e-3 ! mg m-3

        ! Calculate derivative
        doxy_dt = (self%p_vel / 86400.0_rk)*(osat - oxy)
        _SET_SURFACE_DIAGNOSTIC_(self%id_doxy, doxy_dt)

        ! Update state variable
        _ADD_SURFACE_FLUX_(self%id_oxy, doxy_dt)

        _SURFACE_LOOP_END_
    end subroutine do_surface

    subroutine do(self, _ARGUMENTS_DO_)
        !! Performs the pelagic processes of the biogeochemical model
        class(type_imr_norwecom), intent(in) :: self
        _DECLARE_ARGUMENTS_DO_

        ! Local variables
        real(rk) :: temp, nit, pho, sil, sis, det, detp, oxy, dia, fla, par, mes, mic
        real(rk) :: u_max, par_lim, nit_lim, pho_lim, sil_lim
        real(rk) :: dia_growth, dia_resp, dia_mort, fla_growth, fla_resp, fla_mort
        real(rk) :: gpp, npp, chla, gsp, nsp
        real(rk) :: denum, p11, p12, p13, p21, p22, tmp
        real(rk) :: mes_growth_dia, mes_growth_mic, mes_growth_det, mes_loss, mes_resp, mes_mort
        real(rk) :: mic_growth_fla, mic_growth_det, mic_loss, mic_resp, mic_mort
        real(rk) :: dnit_dt, dpho_dt, dsil_dt, dsis_dt, ddet_dt, ddetp_dt, doxy_dt
        real(rk) :: ddia_dt, dfla_dt, dmes_dt, dmic_dt
        real(rk) :: oxy2, dens
        real(rk) :: dsnk, ddsnk_dt

        _LOOP_BEGIN_

        ! Get local copy of the state variables
        _GET_(self%id_temp, temp)
        _GET_(self%id_nit, nit)
        _GET_(self%id_pho, pho)
        _GET_(self%id_sil, sil)
        _GET_(self%id_sis, sis)
        _GET_(self%id_det, det)
        _GET_(self%id_detp, detp)
        _GET_(self%id_oxy, oxy)
        _GET_(self%id_dia, dia)
        _GET_(self%id_fla, fla)
        _GET_(self%id_par, par)
        _GET_(self%id_mes, mes)
        _GET_(self%id_mic, mic)
        _GET_(self%id_dens, dens)
        _GET_(self%id_dsnk, dsnk)

        ! Convert PAR
        par = par / 0.217_rk ! W m-2 -> uE m-2 s-1

        ! Calculate rates
        
        ! Diatoms
        u_max = self%a1*exp(self%a2*temp) ! Temperature dependent growth rate
        par_lim = slim(self%dia_ar, u_max, par) ! Light limitation term
        nit_lim = slim(self%dia_an, u_max, nit) ! Nitrate limitation term
        pho_lim = slim(self%dia_ap, u_max, pho) ! Phosphate limitation term
        sil_lim = slim(self%dia_as, u_max, sil) ! Silicate limitation term
        dia_growth = u_max*min(par_lim, nit_lim, pho_lim, sil_lim) ! Diatoms growth rate
        dia_resp = self%a5*exp(self%a6*temp) ! Diatoms respiration loss rate
        dia_mort = self%cc3 !! Diatoms mortality loss rate

        ! Constrain diatoms loss if dia < dia_min
        if (dia < self%dia_min) then
            dia_resp = 0.0_rk
            dia_mort = 0.0_rk
        end if

        ! Flagellates
        u_max = self%a3*exp(self%a4*temp) ! Temperature dependent growth rate
        par_lim = slim(self%fla_ar, u_max, par) ! Light limitation term
        nit_lim = slim(self%fla_an, u_max, nit) ! Nitrate limitation term
        pho_lim = slim(Self%fla_ap, u_max, pho) ! Phosphate limitation term
        fla_growth = 0.6*u_max*min(par_lim, nit_lim, pho_lim) ! Flagellates growth rate
        fla_resp = self%a5*exp(self%a6*temp) ! Flagellates respiration loss rate
        fla_mort = self%cc3 !! Flagellates mortality loss rate

        ! Constrain flagellate loss if fla < fla_min
        if (fla < self%fla_min) then
            fla_resp = 0.0_rk
            fla_mort = 0.0_rk
        end if

        ! Mesozooplankton
        denum = self%pi11*dia + self%pi12*mic + self%pi13*det + eps ! Total prey concentration
        p11 = self%pi11*dia/denum ! Diatom prey concentration
        p12 = self%pi12*mic/denum ! Microzooplankton prey concentration
        p13 = self%pi13*det/denum ! Detritus prey concentration
        tmp = tfac(temp)*self%mes_g ! Mesozooplankton temperature dependent growth rate
        tmp = tmp/(self%cnit*self%k3 + p11*dia + p12*mic + p13*det) ! Temporary calculation
        mes_growth_dia = tmp*p11*dia ! Diatom eaten by mesozooplankton
        mes_growth_mic = tmp*p12*mic ! Microzooplankton eaten by mesozooplankton
        mes_growth_det = tmp*p13*det ! Detritus eaten by mesozooplankton
        mes_loss = self%mju2*(mes/(mes + self%cnit*self%k6)) ! Mesozooplankton total loss rate
        mes_resp = (1.0_rk - self%delta)*mes_loss ! Mesozooplankton respiration loss rate
        mes_mort = self%delta*mes_loss ! Mesozooplankton mortality loss
        
        ! Contrain predation on diatoms if dia < dia_min
        if (dia < self%dia_min) then
            mes_growth_dia = 0.0_rk
        end if

        ! Microzooplankton
        denum = self%pi21*fla + self%pi22*det + eps ! Total prey concentration
        p21 = self%pi21*fla/denum ! Flagellate prey concentration
        p22 = self%pi22*det/denum ! Detritus prey concentration
        tmp = tfac(temp)*self%mic_g ! Microzooplankton temperature dependent growth rate
        tmp = tmp/(self%cnit*self%k3 + p21*fla + p22*det) ! Temporary calculation
        mic_growth_fla = tmp*p21*fla ! Flagellates eaten by microzooplankton
        mic_growth_det = tmp*p22*det ! Detritus eaten by microzooplankton
        mic_loss = self%mju2*(mic/(mic + self%cnit*self%k6)) ! Microzooplankton total loss
        mic_resp = (1.0_rk - self%delta)*mic_loss ! Microzooplankton respiration loss
        mic_mort = self%delta*mic_loss ! Microzooplankton mortality loss

        ! Contrain predation on flagellates if fla < fla_min
        if (fla < self%fla_min) then
            mic_growth_fla = 0.0_rk
        end if

        ! Calculate diagnostic variables
        gpp = self%scc2*(dia_growth*dia + fla_growth*fla) ! Gross primary production
        npp = gpp - self%scc2*(dia_resp*dia + fla_resp*fla) ! Net primary production
        gsp = self%scc2*(self%beta*mes_growth_dia*mes & ! Gross secondary production
            + self%beta*mes_growth_mic*mes &
            + self%beta*mes_growth_det*mes &
            + self%beta*mic_growth_fla*mic &
            + self%beta*mic_growth_det*mic)
        nsp = gsp & ! Net secondary production
            - self%scc2*(mes_resp*mes &
            + mic_resp*mic)
        chla = (dia + fla)/self%n2chla ! Chlorophyll a concentration
        oxy2 = ((oxy/32.0_rk)/dens)*1000.0_rk ! Oxygen concentration in umol kg-1

        ! Detritus dynamic sinking speed
        if (self%use_dsnk) then
            ddsnk_dt = 0.0_rk
            ddsnk_dt = ddsnk_dt &
                + 0.9_rk*dia_mort*dia*self%sr_dia2det & ! Diatoms mortality (90% is converted to nitrogen detritus)
                + 0.9_rk*fla_mort*fla*self%sr_fla2det & ! Flagellates mortality (90% is converted to nitrogen detritus)
                + mes_mort*mes*self%sr_mes2det & ! Mesozooplankton mortality
                + mic_mort*mic*self%sr_mic2det & ! Microzooplankton mortality
                + (1.0_rk - self%beta)*mes_growth_dia*mes*self%sr_dia2det & ! Mesozooplankton assimilation loss from diatoms
                + (1.0_rk - self%beta)*mes_growth_mic*mes*self%sr_mic2det & ! Mesozooplankton assimilation loss from microzooplankton
                + (1.0_rk - self%beta)*mes_growth_det*mes*dsnk/det & ! Mesozooplankton assimilation loss from detritus
                + (1.0_rk - self%beta)*mic_growth_fla*mic*self%sr_fla2det & ! Microzooplankton assimilation loss from flagellates
                + (1.0_rk - self%beta)*mic_growth_det*mic*dsnk/det & ! Microzooplankton assimilation loss from detritus
                - mes_growth_det*mes*dsnk/det & ! Mesozooplankton predation on detritus
                - mic_growth_det*mic*dsnk/det & ! Microzooplankton predation on detritus
                - self%cc4*dsnk ! Remineralization
        end if

        ! Calculate derivatives

        ! Nitrate
        if (nit > 8000.0_rk) then
            print *, nit, dnit_dt
        end if
        dnit_dt = 0.0_rk
        dnit_dt = dnit_dt &
            + self%cc4*det & ! Remineralization
            + dia_resp*dia & ! Diatoms respiration
            + fla_resp*fla & ! Flagellates respiration
            + 0.1_rk*dia_mort*dia & ! Diatoms mortality (10% is converted directly to nitrate)
            + 0.1_rk*fla_mort*fla & ! Flagellates mortality (10% is converted directly to nitrate)
            + mes_resp*mes & ! Mesozooplankton respiration
            + mic_resp*mic & ! Microzooplankton respiration
            - dia_growth*dia & ! Diatoms growth
            - fla_growth*fla ! Flagellates growth
        
        ! Phosphate
        dpho_dt = 0.0_rk
        dpho_dt = dpho_dt &
            + self%cc4*detp & ! Remineralization
            + self%cc1*(dia_resp*dia & ! Diatoms respiration
            + fla_resp*fla & ! Flagellates respiration
            + 0.25_rk*dia_mort*dia & ! Diatoms mortality (25% is converted directly to phosphate)
            + 0.25_rk*fla_mort*fla & ! Flagellates mortality (25% is converted directly to phosphate)
            + mes_resp*mes & ! Mesozooplankton respiration
            + mic_resp*mic & ! Microzooplankton respiration
            - dia_growth*dia & ! Diatoms growth
            - fla_growth*fla) ! Flagellates growth

        ! Silicate
        dsil_dt = 0.0_rk
        dsil_dt = dsil_dt &
            + self%scc4*sis & ! Remineralization
            - self%cc2*dia_growth*dia ! Diatoms growth

        ! Biogenic silica
        dsis_dt = 0.0_rk
        dsis_dt = dsis_dt &
            + self%cc2*(dia_resp*dia & ! Diatoms respiration
            + fla_resp*fla & ! Flagellates respiration
            + mes_growth_dia*mes) & ! Mesozooplankton predation (biogenic silica is generated instantly)
            - self%scc4*sis ! Remineralization
        
        ! Nitrogen detritus
        ddet_dt = 0.0_rk
        ddet_dt = ddet_dt &
            + 0.9_rk*dia_mort*dia & ! Diatoms mortality (90% is converted to nitrogen detritus)
            + 0.9_rk*fla_mort*fla & ! Flagellates mortality (90% is converted to nitrogen detritus)
            + mes_mort*mes & ! Mesozooplankton mortality
            + mic_mort*mic & ! Microzooplankton mortality
            + (1.0_rk - self%beta)*mes_growth_dia*mes & ! Mesozooplankton assimilation loss from diatoms
            + (1.0_rk - self%beta)*mes_growth_mic*mes & ! Mesozooplankton assimilation loss from microzooplankton
            + (1.0_rk - self%beta)*mes_growth_det*mes & ! Mesozooplankton assimilation loss from detritus
            + (1.0_rk - self%beta)*mic_growth_fla*mic & ! Microzooplankton assimilation loss from flagellates
            + (1.0_rk - self%beta)*mic_growth_det*mic & ! Microzooplankton assimilation loss from detritus
            - mes_growth_det*mes & ! Mesozooplankton predation on detritus
            - mic_growth_det*mic & ! Microzooplankton predation on detritus
            - self%cc4*det ! Remineralization

        ! Phosphorus detritus
        ddetp_dt = 0.0_rk
        ddetp_dt = ddetp_dt &
            + self%cc1*(0.75_rk*dia_mort*dia & ! Diatoms mortality (75% is converted to phosphorus detritus)
            + 0.75_rk*fla_mort*fla & ! Flagellates mortality (75% is converted to phosphorus detritus)
            + mes_mort*mes & ! Mesozooplankton mortality
            + mic_mort*mic & ! Microzooplankton mortality
            + (1.0_rk - self%beta)*mes_growth_dia*mes & ! Mesozooplankton assimilation loss from diatoms
            + (1.0_rk - self%beta)*mes_growth_mic*mes & ! Mesozooplankton assimilation loss from microzooplankton
            + (1.0_rk - self%beta)*mes_growth_det*mes & ! Mesozooplankton assimilation loss from detritus
            + (1.0_rk - self%beta)*mic_growth_fla*mic & ! Microzooplankton assimilation loss from flagellates
            + (1.0_rk - self%beta)*mic_growth_det*mic & ! Microzooplankton assimilation loss from detritus
            - mes_growth_det*mes & ! Mesozooplankton predation on detritus
            - mic_growth_det*mic) & ! Microzooplankton predation on detritus
            - self%cc4*detp ! Remineralization

        ! Oxygen
        doxy_dt = 0.0_rk
        doxy_dt = doxy_dt & !     
!                - self%scc1*dnit_dt ! Contant O/N ratio
            + self%scc1*(dia_growth*dia & ! Diatom production
            + fla_growth*fla & ! Flagellate production
            - dia_resp*dia & ! Diatom respiration
            - fla_resp*fla & ! Flagellate respiration
            - mes_resp*mes & ! Mesozooplankton respiration
            - mic_resp*mic) & ! Microzooplankton respiration
            - self%cc4*det

        ! Diatoms
        ddia_dt = 0.0_rk
        ddia_dt = ddia_dt &
            + dia_growth*dia & ! Diatoms gross growth
            - dia_resp*dia & ! Diatoms respiration loss
            - dia_mort*dia & ! Diatoms mortality loss
            - mes_growth_dia*mes ! Diatoms predation loss from mesozooplankton

        ! Flagellates
        dfla_dt = 0.0_rk
        dfla_dt = dfla_dt &
            + fla_growth*fla & ! Flagellates gross growth
            - fla_resp*fla & ! Flagellates respiration
            - fla_mort*fla & ! Flagellates mortality
            - mic_growth_fla*mic ! Flagellates predation loss from microzooplankton

        ! Mesozooplankton
        dmes_dt = 0.0_rk
        dmes_dt = dmes_dt &
            + self%beta*mes_growth_dia*mes & ! Assimilated from diatom predation
            + self%beta*mes_growth_mic*mes & ! Assimilated from microzooplankton predation
            + self%beta*mes_growth_det*mes & ! Assimilated from detritus predation
            - mes_resp*mes & ! Mesozooplankton respiration loss
            - mes_mort*mes ! Mesozooplankton mortality loss

        ! Microzooplankton
        dmic_dt = 0.0_rk
        dmic_dt = dmic_dt &
            + self%beta*mic_growth_fla*mic & ! Assimilated from flagellates predation
            + self%beta*mic_growth_det*mic & ! Assimilated from detritus predation
            - mes_growth_mic*mes &
            - mic_resp*mic & ! Microzooplankton respiration loss
            - mic_mort*mic ! Microzooplankton mortality

        ! Update state variables
        _ADD_SOURCE_(self%id_nit, dnit_dt)
        _ADD_SOURCE_(self%id_pho, dpho_dt)
        _ADD_SOURCE_(self%id_sil, dsil_dt)
        _ADD_SOURCE_(self%id_sis, dsis_dt)
        _ADD_SOURCE_(self%id_det, ddet_dt)
        _ADD_SOURCE_(self%id_detp, ddetp_dt)
        _ADD_SOURCE_(self%id_oxy, doxy_dt)
        _ADD_SOURCE_(self%id_dia, ddia_dt)
        _ADD_SOURCE_(self%id_fla, dfla_dt)
        _ADD_SOURCE_(self%id_mes, dmes_dt)
        _ADD_SOURCE_(self%id_mic, dmic_dt)
        if (self%use_dsnk) then
            _ADD_SOURCE_(self%id_dsnk, ddsnk_dt)
        end if

        ! Update diagnostic variables
        _SET_DIAGNOSTIC_(self%id_gpp, gpp)
        _SET_DIAGNOSTIC_(self%id_npp, npp)
        _SET_DIAGNOSTIC_(self%id_gsp, gsp)
        _SET_DIAGNOSTIC_(self%id_nsp, nsp)
        _SET_DIAGNOSTIC_(self%id_chla, chla)
        _SET_DIAGNOSTIC_(self%id_oxy2, oxy2)
        if (self%use_dsnk) then
            _SET_DIAGNOSTIC_(self%id_snkspd, (dsnk/det)*86400.0_rk)
        end if

        _LOOP_END_

    contains
        
        function slim(alpha, pmax, s) result(lim)
            !! Returns the substrate limitation term
            real(rk), intent(in) :: alpha !! Substrate affinity
            real(rk), intent(in) :: pmax !! Production max
            real(rk), intent(in) :: s !! Substrate concentration
            real(rk) :: lim
            
            if (s < 0.0_rk) then
                lim = 0.0_rk
            else
                lim = s/(s + (pmax/alpha))
            end if
        end function slim

        function tfac(te) result(t_dep)
            !! Returns the temperature dependence on zooplankton growth
            real(rk), intent(in) :: te !! Temperature (degC)
            real(rk) :: t_dep !! Temperature dependency scalar
            t_dep = self%q10**((te - 10.0_rk)/10.0_rk)
        end function tfac

    end subroutine do

    subroutine do_bottom(self, _ARGUMENTS_DO_BOTTOM_)
        class(type_imr_norwecom), intent(in) :: self
        _DECLARE_ARGUMENTS_DO_BOTTOM_

        ! Local variables
        real(rk) :: bstress, detspd, sisspd
        real(rk) :: botdet, botdetp, botsis, burdet, burdetp, bursis
        real(rk) :: det, detp, sis, dsnk
        real(rk) :: ddet_dt, ddetp_dt, dsis_dt
        real(rk) :: dbotdet_dt, dbotdetp_dt, dbotsis_dt
        real(rk) :: dburdet_dt, dburdetp_dt, dbursis_dt

        _BOTTOM_LOOP_BEGIN_

        ! Get local copy of state variables
        _GET_HORIZONTAL_(self%id_bstress, bstress)
        _GET_HORIZONTAL_(self%id_botdet, botdet)
        _GET_HORIZONTAL_(self%id_botdetp, botdetp)
        _GET_HORIZONTAL_(self%id_botsis, botsis)
        _GET_HORIZONTAL_(self%id_burdet, burdet)
        _GET_HORIZONTAL_(self%id_burdetp, burdetp)
        _GET_HORIZONTAL_(self%id_bursis, bursis)
        _GET_(self%id_det, det)
        _GET_(self%id_detp, detp)
        _GET_(self%id_sis, sis)
        if (self%use_dsnk) then
            _GET_(self%id_dsnk, dsnk)
        end if

        ! Set sedimentation/resuspension rate according to bottom stress
        if (bstress < self%tau1) then ! Sedimentation
            if (self%use_dsnk) then
                detspd = dsnk/det
                sisspd = self%sr_sis
            else
                detspd = self%sr_det
                sisspd = self%sr_sis
            end if
        else
            detspd = 0.0_rk
            sisspd = 0.0_rk
        end if

        ! Calculate derivatives
        ddet_dt = detspd*det
        ddetp_dt = detspd*detp
        dsis_dt = sisspd*sis
        dburdet_dt = self%scc8*max(0.0_rk, botdet - self%det_bul)
        dburdetp_dt = self%scc8*max(0.0_rk, botdetp - self%detp_bul)
        dbursis_dt = self%scc8*max(0.0_rk, botsis - self%sis_bul)
        dbotdet_dt = ddet_dt - dburdet_dt
        dbotdetp_dt = ddetp_dt - dburdetp_dt
        dbotsis_dt = dsis_dt - dbursis_dt

        ! Update state variables
        _ADD_BOTTOM_FLUX_(self%id_det, -ddet_dt)
        _ADD_BOTTOM_FLUX_(self%id_detp, -ddetp_dt)
        _ADD_BOTTOM_FLUX_(self%id_sis, -dsis_dt)
        if (self%use_dsnk) then
            _ADD_BOTTOM_FLUX_(self%id_dsnk, -detspd*det*dsnk/det)
        end if
        _ADD_BOTTOM_SOURCE_(self%id_botdet, dbotdet_dt)
        _ADD_BOTTOM_SOURCE_(self%id_botdetp, dbotdetp_dt)
        _ADD_BOTTOM_SOURCE_(self%id_botsis, dbotsis_dt)
        _ADD_BOTTOM_SOURCE_(self%id_burdet, dburdet_dt)
        _ADD_BOTTOM_SOURCE_(self%id_burdetp, dburdetp_dt)
        _ADD_BOTTOM_SOURCE_(self%id_bursis, dbursis_dt)

        _BOTTOM_LOOP_END_
    end subroutine do_bottom

    subroutine get_vertical_movement(self, _ARGUMENTS_GET_VERTICAL_MOVEMENT_)
        !! Sets the sinking speed
        class(type_imr_norwecom), intent(in) :: self !! NORWECOM model class
        _DECLARE_ARGUMENTS_GET_VERTICAL_MOVEMENT_

        ! Local variables
        real(rk) :: sil, dia, fla, det, dsnk
        real(rk) :: vdia, vfla, vdet

        _LOOP_BEGIN_

        ! Get local copy of the state variables
        _GET_(self%id_sil, sil)
        _GET_(self%id_dia, dia)
        _GET_(self%id_fla, fla)
        if (self%use_dsnk) then
            _GET_(self%id_det, det)
            _GET_(self%id_dsnk, dsnk)
        end if

        ! Adjust diatoms sinking speed according to silicate concentration
        if ((sil / self%cnit) < self%sib) then
            vdia = self%sr_dia_max
        else
            vdia = self%sr_dia_min + (self%sr_dia_max - self%sr_dia_min)/(sil/self%csil)
        end if

        ! Constrain sinking if concentrations are too low
        if (dia < self%dia_min) then
            vdia = 0.0_rk
        end if
        if (fla < self%fla_min) then
            vfla = 0.0_rk
        else
            vfla = self%sr_fla
        end if

        ! Update sinking speeds
        _SET_VERTICAL_MOVEMENT_(self%id_dia, -vdia)
        _SET_VERTICAL_MOVEMENT_(self%id_fla, -vfla)
        _SET_VERTICAL_MOVEMENT_(self%id_sis, -self%sr_sis)
        if (self%use_dsnk) then
            vdet = dsnk/det
            _SET_VERTICAL_MOVEMENT_(self%id_det, -vdet)
            _SET_VERTICAL_MOVEMENT_(self%id_detp, -vdet)
            _SET_VERTICAL_MOVEMENT_(self%id_dsnk, -vdet)
        else
            _SET_VERTICAL_MOVEMENT_(self%id_det, -self%sr_det)
            _SET_VERTICAL_MOVEMENT_(self%id_detp, -self%sr_det)
        end if

        _LOOP_END_
    end subroutine get_vertical_movement

    subroutine get_light_extinction(self, _ARGUMENTS_GET_EXTINCTION_)
        !! Sets the light extinction coefficient
        class(type_imr_norwecom), intent(in) :: self
        _DECLARE_ARGUMENTS_GET_EXTINCTION_

        ! Local variables
        real(rk) :: dia, fla, my_extinction

        _LOOP_BEGIN_

        ! Get local copy of state variables
        _GET_(self%id_dia, dia)
        _GET_(self%id_fla, fla)

        ! Calculate extinction from chlorophyll a
        my_extinction = self%v*((dia + fla)/self%n2chla)

        ! Update extinction
        _SET_EXTINCTION_(my_extinction)

        _LOOP_END_
    end subroutine get_light_extinction

end module imr_norwecom
