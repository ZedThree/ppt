!----------------------------------------------------
!> This module handles the electromagnetic fields.
!----------------------------------------------------
MODULE fields

  IMPLICIT none

  ! calculate the fields

  CONTAINS

    !----------------------------------------------------
    !> Get the magnitude of the magnetic field B at 
    !> position (x, y, z)
    !----------------------------------------------------
    SUBROUTINE get_B_mag(position, B)
      
      !> Position coordinates
      REAL, DIMENSION(3), INTENT(IN) :: position
      !> Magnetic field strength
      REAL, INTENT(OUT)	:: B

      ! Nice and hard-coded for now.
      ! At some point, will need to be able to choose between
      ! functions and so on.
      B = 1

    END SUBROUTINE get_B_mag

    !----------------------------------------------------
    !> Get the direction b=(bx, by, bz) of the magnetic
    !> field B at position (x, y, z)
    !----------------------------------------------------
    SUBROUTINE get_B_vec(position, b)
      
      !> Position coordinates
      REAL, DIMENSION(3), INTENT(IN)  :: position
      !> Magnetic field unit vector
      REAL, DIMENSION(3), INTENT(OUT) :: b
      ! Make a derived type with fields b%x, b%y, b%z?

      ! Nice and hard-coded for now.
      ! At some point, will need to be able to choose between
      ! functions and so on.
      b = (/0., 0., 1./)
      
    END SUBROUTINE get_B_vec
    
END MODULE fields
