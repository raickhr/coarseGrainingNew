module filterModule

    use kinds

    contains

    subroutine filterScalarFieldOnSphere(padsize, ell_filter, field2D, filtered_field2D, LAT, LON, AREA)
        integer(kind = int_kind), intent(in), dimension(:) :: padsize
        real(kind = real_kind), intent(in) :: ell_filter
        real(kind = real_kind), intent(in), dimension(:,:,:):: field2D
        real(kind = real_kind), intent(out), dimension(:,:,:), allocatable :: filtered_field2D
        real(kind = real_kind), intent(in), dimension(:,:):: LAT, LON, AREA

        integer:: field_shape(3)

        integer:: grid_shape(2)

        integer :: num_of_field2D, nx, ny,  &
                   x_start, x_end, &
                   y_start, y_end

        integer :: ierr

        field_shape = shape(field2D)
        nx = field_shape(1)
        ny = field_shape(2)
        num_of_field2D = field_shape(3)

        ! Check shape with LAT , LON and AREA
        grid_shape = shape(LAT)
        
        if (nx .NE. grid_shape(1) ) stop 'LAT shape and field shape mismatch'
        if (ny .NE. grid_shape(2) ) stop 'LAT shape and field shape mismatch'
        
        grid_shape = shape(LON)
        
        if (nx .NE. grid_shape(1) ) stop 'LON shape and field shape mismatch'
        if (ny .NE. grid_shape(2) ) stop 'LON shape and field shape mismatch'
        
        
        grid_shape = shape(AREA)
        
        if (nx .NE. grid_shape(1) ) stop 'AREA shape and field shape mismatch'
        if (ny .NE. grid_shape(2) ) stop 'AREA shape and field shape mismatch'

        allocate(filtered_field2D(nx, ny, num_of_field2D), stat = ierr)
        
        
        x_start = padsize(1) + 1
        x_end = nx - padsize(2)
        y_start = padsize(3) + 1
        y_end = ny - padsize(4)




        

    end subroutine





end module filterModule