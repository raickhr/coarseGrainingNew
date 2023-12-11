module test
    use kinds
    use weight_objects
    implicit none

    contains

        subroutine mysubroutine(i, j, n, obj)
            integer, intent(in) ::i , j,  n
            type(pointWeight), intent(out) :: obj

            integer :: i_indices(n), j_indices(n)
            integer :: a
            real(kind = real_kind) :: weights(n)

            i_indices = (/(a, a=1, n, 1)/)
            j_indices = (/(a, a=1, n, 1)/) !(/(I, I=1, n, 1)/)
            
            call obj%fillIn( i, j, n , i_indices, j_indices, weights )

        end subroutine

end module
