!-----------------------------------------------
!> This module handles the dipoles
!-----------------------------------------------
MODULE dipole

  USE globals
  USE normalisations

  IMPLICIT NONE

CONTAINS
  
  !-----------------------------------------------
  !> Converts position vector from cartesian to
  !> spherical polar coordinates
  !-----------------------------------------------
  SUBROUTINE cart_to_sph_polar(vector_cart, vector_polar)
   
    !> vector in cartesian coordinates (x, y, z)
    REAL, DIMENSION(3), INTENT(in)  :: vector_cart
    !> vector in spherical polar coordinates (r, theta, phi)
    REAL, DIMENSION(3), INTENT(out) :: vector_polar

    ! dummy variables
    !> radius
    REAL :: r
    !> inclination angle
    real :: theta
    !> azimuthal angle
    REAL :: phi

    ! radius
    r = SQRT(vector_cart(1)**2 + vector_cart(2)**2 + vector_cart(3)**2)
    ! inclination angle
    theta = ACOS(vector_cart(3) / r)
    ! azimuthal angle
    phi = ATAN2(vector_cart(2), vector_cart(1))

    ! resultant vector
    vector_polar = (/ r, theta, phi /)
    
  END SUBROUTINE cart_to_sph_polar

  !-----------------------------------------------
  !> Converts position vector from spherical polar
  !> to cartesian coordinates
  !-----------------------------------------------
  SUBROUTINE sph_polar_to_cart(vector_polar, vector_cart)

    !> vector in spherical polar coordinates (r, theta, phi)
    REAL, DIMENSION(3), INTENT(in) :: vector_polar
    !> vector in cartesian coordinates (x, y, z)
    REAL, DIMENSION(3), INTENT(out)  :: vector_cart

    ! dummy variables
    !> radius
    REAL :: r
    !> inclination angle
    real :: theta
    !> azimuthal angle
    REAL :: phi

    r     = vector_polar(1)
    theta = vector_polar(2)
    phi   = vector_polar(3)

    vector_cart(1) = r * SIN(theta) * COS(phi)
    vector_cart(2) = r * SIN(theta) * SIN(phi)
    vector_cart(3) = r * COS(theta)

  END SUBROUTINE sph_polar_to_cart
  
  !-----------------------------------------------
  !> Get the magnetic field due to a dipole at 
  !> position, by first switching to spherical
  !> polar coordinates
  !-----------------------------------------------
  SUBROUTINE dipole_field(position, dipole_in, B)

    !> position of particle
    REAL, DIMENSION(3), INTENT(in)  :: position
    !> dipole object
    TYPE(dipole_obj), INTENT(in)    :: dipole_in
    !> resulant magnetic field
    REAL, DIMENSION(3), intent(out) :: B
    !> magnetic field in spherical polars
    REAL, dimension(3)		    :: B_polar

    ! dummy variables
    !> particle position in spherical polar coords
    REAL, DIMENSION(3) :: position_polar
    !> magnetic field prefactor
    real :: prefactor

    CALL cart_to_sph_polar(position, position_polar)

    prefactor = (mu0 * dipole_in%size) / 4 * pi
    prefactor = prefactor / position_polar(1)**3
    
    B_polar = (/ 2 * COS(position_polar(2)), SIN(position_polar(2)), 0. /)

    B_polar = prefactor * B_polar

    CALL sph_polar_to_cart(B_polar, B)
  
  END SUBROUTINE dipole_field

  !> Initialise the two dipoles
  SUBROUTINE init_dipole(separation, size, dipole1, dipole2)
    
    !> The distance separating the dipoles
    REAL, intent(in) :: separation
    !> The magnetic moment of the dipoles
    REAL, intent(in) :: size

    !> the two dipoles
    TYPE(dipole_obj) :: dipole1, dipole2

    !> dummy
    real :: y

    y = separation / 2.

    dipole1%position = (/ 0., y, 0./)
    dipole1%size     = size

    dipole2%position = (/ 0., -1.*y, 0./)
    dipole2%size = size

  END SUBROUTINE init_dipole
  

END MODULE dipole

  
