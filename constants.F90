module constants

    use kinds
    
    implicit none
    save

!-----------------------------------------------------------------------
!
!     physical constants (all in cgs units except for those in MKS)
!
!-----------------------------------------------------------------------

    real (kind=real_kind), parameter ::     &
    &  grav          = 9.806_real_kind,          & ! gravit. accel. (m/s**2)
    &  omega         = 7.292123625e-5_real_kind, & ! angular vel. of Earth 1/s
    &  radius_earth  = 6370.0e3_real_kind,       & ! radius of Earth (m)
    &  rho_sw        = 1026_real_kind,           & ! density of sea water (g/cm^3)
    &  rho_fw        = 1000_real_kind              ! density of pure water(g/cm^3)

    end module constants

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||