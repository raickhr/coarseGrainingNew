module weightAndIndices
    use kinds
    use filterModule
    implicit NONE

    contains
    subroutine calcWeightAndIndices(ell_filter, LAT, LON, AREA, i_indices, j_indices, out1d_weight)
        real(kind = real_kind), intent(in), dimension(:,:) :: LAT, LON, AREA
        real(kind = real_kind), intent(in) :: ell_filter

        call getMaskedArrAndWeightByDist(ell_filter, center_lat, center_lon,              &
                                         in2dlat, in2dlon, in2darea                       &
                                         out1d_i_index, out1d_j_index, out1d_weight)
    end subroutine calcWeightAndIndices
end module weightAndIndices