module filter_module
    use kinds
    use constants
    use weight_module

    implicit none

    contains

    real(kind=real_kind) function filter_at_ij(n, field_in_1d, weight_in_1d)
        integer, intent(in) :: n
        real(kind=real_kind), intent(in) :: field_in_1d(n), weight_in_1d(n)
        filter_at_ij = sum(field_in_1d(:) * weight_in_1d(:))
    end function filter_at_ij

    

    ! subroutine filterScalarFieldOnSphere(padsize, ell_filter, field2D, filtered_field2D, LAT, LON, AREA)
    !     integer(kind = int_kind), intent(in), dimension(:) :: padsize
    !     real(kind = real_kind), intent(in) :: ell_filter
    !     real(kind = real_kind), intent(in), dimension(:,:,:):: field2D
    !     real(kind = real_kind), intent(out), dimension(:,:,:), allocatable :: filtered_field2D
    !     real(kind = real_kind), intent(in), dimension(:,:):: LAT, LON, AREA

    !     integer:: field_shape(3)

    !     integer:: grid_shape(2)

    !     integer :: num_of_field2D, nx, ny,  &
    !                x_start, x_end, &
    !                y_start, y_end

    !     integer :: ierr, i_counter, j_counter

    !     field_shape = shape(field2D)
    !     nx = field_shape(1)
    !     ny = field_shape(2)
    !     num_of_field2D = field_shape(3)

    !     ! Check shape with LAT , LON and AREA
    !     grid_shape = shape(LAT)
        
    !     if (nx .NE. grid_shape(1) ) stop 'LAT shape and field shape mismatch'
    !     if (ny .NE. grid_shape(2) ) stop 'LAT shape and field shape mismatch'
        
    !     grid_shape = shape(LON)
        
    !     if (nx .NE. grid_shape(1) ) stop 'LON shape and field shape mismatch'
    !     if (ny .NE. grid_shape(2) ) stop 'LON shape and field shape mismatch'
        
        
    !     grid_shape = shape(AREA)
        
    !     if (nx .NE. grid_shape(1) ) stop 'AREA shape and field shape mismatch'
    !     if (ny .NE. grid_shape(2) ) stop 'AREA shape and field shape mismatch'

    !     allocate(filtered_field2D(nx, ny, num_of_field2D), stat = ierr)
        
        
    !     x_start = padsize(1) + 1
    !     x_end = nx - padsize(2)
    !     y_start = padsize(3) + 1
    !     y_end = ny - padsize(4)

    !     do j_counter=y_start, y_end
    !         do i_counter=x_start, x_end
    !             call filterAtIJ(filtered_field2D(i_counter, j_counter,:), &
    !                             ell_filter, field2D, &
    !                             LAT(i_counter, j_counter), &
    !                             center_LON(i_counter, j_counter), &
    !                             LAT, LON, AREA)
    !         end do
    !     end do

    ! end subroutine
    

end module filter_module