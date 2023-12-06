! This module defines the data type for common data types
module kinds
    implicit none
    integer, parameter :: char_len = 80, &
                        & int_kind = kind(1), &
                        & log_kind = kind(.true.), &
                        & real_kind = selected_real_kind(13), &
                        & half_float_kind = selected_real_kind(6)
end module
