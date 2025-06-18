module imr_model_library

    use fabm_types, only: type_base_model_factory, type_base_model

    implicit none

    private

    type, extends(type_base_model_factory) :: type_factory
    contains
        procedure :: create
    end type

    type(type_factory), save, target, public :: imr_model_factory

contains

    subroutine create(self, name, model)

        use imr_norwecom
        use imr_norwecom_num

        class(type_factory), intent(in) :: self
        character(*), intent(in) :: name
        class(type_base_model), pointer :: model

        select case(name)
            case("norwecom"); allocate(type_imr_norwecom::model)
            case("norwecom_num"); allocate(type_imr_norwecom_num::model)
        end select
    end subroutine create

end module imr_model_library