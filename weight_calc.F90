module weight_calc

    use kinds
    use operators

    implicit NONE

    real(kind=real_kind), dimension(:), allocatable  :: out1d_i_index, &
                                                        out1d_j_index, &
                                                        out1d_weight


    contains

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

        greatCircleDistance = greatCircleDistance / 1d3  ! changing to KM from meters
        ell_filterInKM = ell_filter / 1d3  ! changing to KM from meters

        workArr(:,:) = 0.5 
        kernel = workArr-0.5*dtanh((greatCircleDistance-(ell_filterInKM/2))/10.0)

        weight2d = (kernel * in2darea)/sum(kernel * in2darea)

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

end module weight_calc