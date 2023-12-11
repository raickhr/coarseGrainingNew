module weight_objects

    use constants
    use kinds

    type pointWeight

        integer :: point_i_index, point_j_index

        real(kind=real_kind), dimension(:), allocatable  :: out1d_i_index, &
                                                            out1d_j_index, &
                                                            out1d_weight
        contains
            procedure :: fillIn

    end type pointWeight

    interface pointWeight
        MODULE PROCEDURE :: constructor
    end interface pointWeight

    contains

        function constructor(i, j) result(this)
            integer, intent(in) :: i,j
            type(pointWeight) :: this
            this%point_i_index = i
            this%point_j_index = j
        end function

        subroutine fillIn(this, n , i_indices, j_indices, weights )
            class(pointWeight), intent(inout) :: this
            integer, intent(in):: n
            integer, intent(in) :: i_indices(n), j_indices(n)
            real(kind = real_kind), intent(in) :: weights(n)

            integer :: i_err
            allocate(this%out1d_i_index(n), &
                     this%out1d_j_index(n), &
                     this%out1d_weight(n), stat =i_err)

            this%out1d_i_index = i_indices
            this%out1d_j_index = j_indices
            this%out1d_weight = weights

        end subroutine


    

end module weight_objects