module workDivision
    use kinds
    use constants
    implicit NONE

    contains


    subroutine getMaskedArrAndWeightByDist(center_lat, center_lon,                          &
                                           in2dlat, in2dlon, in2darea                       &
                                           out1d_i_index, out1d_j_index, out1d_weight)


        real(kind=real_kind), intent(in) ::  center_lat, center_lon
        real(kind=real_kind), dimension(:,:), intent(in) ::  in2dlat, in2dlon, in2darea
        real(kind=real_kind), dimension(:), allocatable, intent(out) :: out1d_i_index, out1d_j_index, out1d_weight

        integer :: sizeArr, nx, ny, err_stat, x_counter, y_counter
        logical, allocatable, dimension(:) :: mask
        integer, allocatable, dimension(:) :: flattenedIndices, lat_1d_indices, lon_1d_indices
        integer, allocatable, dimension(:,:) :: lat_indices, lon_indices

        sizeArr = size(in2dlat)
        nx = size(in2dlat, dim=1)
        ny = size(in2dlat, dim=2)

        allocate(flattenedIndices(sizeArr), &
                 lat_indices(nx,ny),
                 lon_indices(nx,ny),
                 lat_1d_indices(ny),
                 lon_1d_indices(nx)
                 stat=i_err)

        lat_1d_indices = (/(I, I=1, ny, 1)/)
        lon_1d_indices = (/(I, I=1, nx, 1)/)

        
        do x_counter = 1, nx
            lat_indices(x_counter,:) = lat_1d_indices(:)
        end do
        
        do y_counter = 1, ny
            lon_indices(:,y_counter) = lon_1d_indices(:)
        end do

        
    end subroutine getMaskedArr

end module