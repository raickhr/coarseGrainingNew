module operators
    use kinds
    use constants

    implicit NONE


    contains

    ! SUBROUTINE  Spher_to_Cart_generic(U_cart,V_cart,W_cart,U_sph,V_sph,W_sph,nx,ny)

    !     IMPLICIT NONE
    
    !     INTEGER, Intent (in) :: nx,ny
    !     REAL(kind=real_kind), Intent (in):: U_sph(nx,ny),V_sph(nx,ny),W_sph(nx,ny)
    !     REAL(kind=real_kind), Intent (out) :: U_cart(nx,ny),V_cart(nx,ny),W_cart(nx,ny)
    !     !Integer :: IntegerI_HA(nx,ny),IntegerII_HA(nx,ny)
    !     !REAL(kind=8) :: WorkI_HA(nx,ny)
    !     INTEGER	:: i,j
    !     REAL ::longitude_increment
    
    
    
    
    !     U_cart = 0
    !     V_cart = 0
    !     W_cart = 0
    
    !     U_cart = W_sph*dcos(dble(ULONG))*dcos(dble(ULAT)) - U_sph*dsin(dble(ULONG)) - V_sph*dcos(dble(ULONG))*dsin(dble(ULAT))
    !     V_cart = W_sph*dsin(dble(ULONG))*dcos(dble(ULAT)) + U_sph*dcos(dble(ULONG)) - V_sph*dsin(dble(ULONG))*dsin(dble(ULAT))
    !     W_cart = W_sph*dsin(dble(ULAT)) + V_sph*dcos(dble(ULAT))
    
    ! END SUBROUTINE  Spher_to_Cart_generic

    ! SUBROUTINE  Cart_to_Spher_generic(U_sph,V_sph,W_sph,U_cart,V_cart,W_cart,nx,ny)

    !     IMPLICIT NONE
      
    !     INTEGER, Intent (in) :: nx,ny
    !     REAL(kind=real_kind), Intent (in)	:: U_cart(nx,ny),V_cart(nx,ny),W_cart(nx,ny)
    !     REAL(kind=real_kind), Intent (out):: U_sph(nx,ny),V_sph(nx,ny),W_sph(nx,ny)
    !     !Integer :: IntegerI_HA(nx,ny),IntegerII_HA(nx,ny)
    !     !REAL(kind=8):: WorkI_HA(nx,ny)
    !     INTEGER :: i,j
    !     REAL ::longitude_increment
      
      
    !     U_sph = 0
    !     V_sph = 0
    !     W_sph = 0
      
    !     W_sph =  U_cart*dcos(dble(ULONG))*cos(dble(ULAT)) + V_cart*dsin(dble(ULONG))*cos(dble(ULAT)) + W_cart*dsin(dble(ULAT))
    !     U_sph = -U_cart*dsin(dble(ULONG)) + V_cart*dcos(dble(ULONG))
    !     V_sph = -U_cart*dcos(dble(ULONG))*dsin(dble(ULAT)) - V_cart*dsin(dble(ULONG))*dsin(dble(ULAT)) + W_cart*cos(dble(ULAT))
      
      
    ! END SUBROUTINE  Cart_to_Spher_generic
      
    
    ! SUBROUTINE log_to_Sph(U_Sph,V_Sph,U_log,V_log,nx,ny)
    !     real(kind=real_kind), intent(in) :: U_log(nx,ny), V_log(nx,ny)
    !     !integer, intent(in) :: nx, ny
    !     real(kind=real_kind), intent(out) :: U_Sph(nx,ny), V_Sph(nx,ny)
      
      
    !     U_Sph = U_log * dcos(dble(ANGLE)) - V_log * dsin(dble(ANGLE))
    !     V_Sph = U_log * dsin(dble(ANGLE)) + V_log * dcos(dble(ANGLE))
      
      
    ! END SUBROUTINE log_to_Sph


    SUBROUTINE getDistance(center_LAT, center_LON, LAT, LON, greatCircleDistance)
        real(kind = real_kind):: center_LAT, center_LON
        real(kind=real_kind), dimension(:,:) :: LAT, LON, greatCircleDistance 

        real(kind=real_kind), dimension(:,:), allocatable :: dlambda, phi1, phi2,   &
                                                             numerator, denominator,&
                                                             dsigma

        integer :: arrShape(2)
        integer :: nx, ny

        arrShape = shape(LAT)

        nx = arrShape(1)
        ny = arrShape(2)

        allocate(dlambda(nx, ny), phi1(nx, ny), phi2(nx, ny), numerator(nx, ny), denominator(nx, ny))

        dlambda = LON - center_LON
        phi1(:,:) = center_LAT
        phi2 = LAT

        numerator = ( cos(phi2)*sin(dlambda) )**2 + (cos(phi1)*sin(phi2) -sin(phi1)*cos(phi2)*cos(dlambda))**2
        numerator = sqrt(numerator)
        denominator = sin(phi1)*sin(phi2) + cos(phi1)*cos(phi2)*cos(dlambda)

        dsigma = atan2(numerator, denominator)
        greatCircleDistance = radius_earth * dsigma

    end SUBROUTINE getDistance


    ! SUBROUTINE getDistance1D(center_LAT, center_LON, LAT, LON, greatCircleDistance)
    !     real(kind = real_kind):: center_LAT, center_LON
    !     real(kind=real_kind), dimension(:) :: LAT, LON, greatCircleDistance 

    !     real(kind=real_kind), dimension(:), allocatable :: dlambda, phi1, phi2,   &
    !                                                        numerator, denominator,&
    !                                                        dsigma

    !     integer :: arrLen

    !     arrLen = size(LAT)

    !     allocate(dlambda(arrLen), phi1(arrLen), phi2(arrLen), numerator(arrLen), denominator(arrLen))

    !     dlambda = LON - center_LON
    !     phi1(:) = center_LAT
    !     phi2(:) = LAT(:)

    !     numerator = ( cos(phi2)*sin(dlambda) )**2 + (cos(phi1)*sin(phi2) -sin(phi1)*cos(phi2)*cos(dlambda))**2
    !     numerator = sqrt(numerator)
    !     denominator = sin(phi1)*sin(phi2) + cos(phi1)*cos(phi2)*cos(dlambda)

    !     dsigma = atan2(numerator, denominator)
    !     greatCircleDistance = earth_radius * dsigma

    ! end SUBROUTINE getDistance


end module