module filterModule

    use kinds
    use operators

    implicit NONE

    real(kind=real_kind), dimension(:), allocatable  :: out1d_i_index, &
                                                        out1d_j_index, &
                                                        out1d_weight


    contains

    ! SUBROUTINE filterAtIJ(filtered_fields_atIJ, ell_filter, field2D, center_LAT, center_LON, LAT, LON, AREA)
    !     real(kind = real_kind), dimension(:,:,:), intent(in) :: field2D
    !     real(kind = real_kind), dimension(:,:), intent(in) :: LAT, LON, AREA
    !     real(kind = real_kind) :: ell_filter, center_LAT, center_LON


    !     real(kind = real_kind), dimension(:,:), allocatable :: distance, weight, &
    !                                                            weightedArea, kernelArea, kernel, &
    !                                                            workArr

    !     real(kind = real_kind), dimension(:), intent(inout) :: filtered_fields_atIJ
            

    !     integer :: arrShape(3), nx, ny, nfields, ierr, counter_k

    !     arrShape = shape(field2D)

    !     nx =  arrShape(1)
    !     ny = arrShape(2)
    !     nfields = arrShape(3)

    !     allocate(distance(nx, ny),     &
    !              weight(nx, ny),       &
    !              workArr(nx, ny),      &
    !              weightedArea(nx, ny), &
    !              kernelArea(nx, ny),   &
    !              kernel(nx, ny),       &
    !              stat=ierr)

    !     call getDistance(center_LAT, center_LON, LAT, LON, distance)

    !     workArr(:,:) = 0.5

    !     distance = distance * 1d3  ! changing to KM from meters
    !     ell_filter = ell_filter * 1d3  ! changing to KM from meters


    !     kernel = workArr-0.5*dtanh((distance-(ell_filter/2))/10.0)
    !     weightedArea = kernel * AREA
    !     kernelArea = sum(weightedArea)

    !     weight = weightedArea(:,:)/kernelArea

    !     do counter_k = 1, nfields
    !         filtered_fields_atIJ(counter_k) = sum(weight(:,:) * field2D(:,:,counter_k))
    !     end do

    ! end SUBROUTINE filterAtIJ

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

    subroutine getMaskedArrAndWeightByDist(ell_filter, center_lat, center_lon,               &
                                           in2dlat, in2dlon, in2darea)!                       &
                                           !out1d_i_index, out1d_j_index, out1d_weight)


        real(kind=real_kind), intent(in) ::  ell_filter, center_lat, center_lon
        real(kind=real_kind), dimension(:,:), intent(in) ::  in2dlat, in2dlon, in2darea
        ! real(kind=real_kind), dimension(:), allocatable, intent(out) :: out1d_i_index, &
        !                                                                 out1d_j_index, &
        !                                                                 out1d_weight

        integer :: nx, ny, err_stat, x_counter, y_counter, sizeMasked, i_err, I
        logical, allocatable, dimension(:,:) :: mask2d
        real(kind = real_kind), allocatable, dimension(:,:) :: weight2d, kernel, workArr, greatCircleDistance
        integer, allocatable, dimension(:) :: lat_1d_indices, lon_1d_indices
        integer, allocatable, dimension(:,:) :: lat_indices, lon_indices

        real(kind = real_kind) :: ell_filterInKM

        nx = size(in2dlat, dim=1)
        ny = size(in2dlat, dim=2)

        allocate(lat_1d_indices(ny),     &
                 lon_1d_indices(nx),      &
                 stat=i_err)

        lat_1d_indices = (/(I, I=1, ny, 1)/)
        lon_1d_indices = (/(I, I=1, nx, 1)/)


        allocate(greatCircleDistance(nx,ny),    &
                 mask2d(nx,ny),                 &
                 weight2d(nx,ny),              &
                 workArr(nx,ny),                &
                 stat=i_err)

        call getDistance(center_lat, center_lon, in2dlat, in2dlon, greatCircleDistance)
        mask2d = greatCircleDistance .LT. (1.1 * (ell_filter/2) ) ! 10% tolerance

        sizeMasked = count(mask2d)
        print *, sizeMasked


        greatCircleDistance = greatCircleDistance / 1d3  ! changing to KM from meters
        ell_filterInKM = ell_filter / 1d3  ! changing to KM from meters

        workArr(:,:) = 0.5 
        kernel = workArr-0.5*dtanh((greatCircleDistance-(ell_filterInKM/2))/10.0)

        weight2d = (kernel * in2darea)/sum(kernel * in2darea)

        ! where (greatCircleDistance < (1.1 * ell_filter ))  ! 10% tolerance
        !     mask2d = .TRUE.
        ! elsewhere
        !     mask2d = .FALSE.
        ! end where

        if (allocated(out1d_i_index)) then
            deallocate(out1d_i_index,  &
                       out1d_j_index,  &
                       out1d_weight,   &
                       stat=err_stat)
        end if
        
        allocate(out1d_i_index(sizeMasked),  &
                 out1d_j_index(sizeMasked),  &
                 out1d_weight(sizeMasked),   &
                 stat=err_stat)
        
        allocate(lat_indices(nx,ny),    &
                 lon_indices(nx,ny),    &
                 stat=i_err)


        do x_counter = 1, nx
            lat_indices(x_counter,:) = lat_1d_indices(:)
        end do

        do y_counter = 1, ny
            lon_indices(:,y_counter) = lon_1d_indices(:)
        end do

        out1d_i_index = pack(lon_indices, mask=mask2d)
        out1d_j_index = pack(lat_indices, mask=mask2d)
        out1d_weight = pack(weight2d, mask = mask2d)
    
    end subroutine getMaskedArrAndWeightByDist

    ! function getIindices() result(i_indices)
    !     real(kind = real_kind), allocatable, dimension(:) :: i_indices

    !     integer :: len, i_err
    !     len = size(out1d_i_index)

    !     allocate(i_indices(len), stat=i_err)
    !     i_indices = out1d_i_index
    !     return

    ! end function getIindices


end module filterModule