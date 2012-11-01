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
    
    !----------------------------------------------------
    !> Get the full magnetic field vector at position
    !----------------------------------------------------
    SUBROUTINE get_B(position, B)
      
      !> Position coordinates
      REAL, DIMENSION(3), INTENT(IN)  :: position
      !> Magnetic field vector
      REAL, DIMENSION(3), INTENT(OUT) :: B
      !> Magnetic field unit vector
      REAL, DIMENSION(3)	      :: b_vec
      !> Magnetic field magnitude
      REAL 			      :: B_mag

      CALL get_B_mag(position, B_mag)
      CALL get_B_vec(position, b_vec)

      B = B_mag*b_vec
      
    END SUBROUTINE get_B

    !----------------------------------------------------
    !> Get the magnitude of the electric field E at 
    !> position
    !----------------------------------------------------
    SUBROUTINE get_E_mag(position, E)
      
      !> Position coordinates
      REAL, DIMENSION(3), INTENT(IN) :: position
      !> Electric field magnitude
      REAL, INTENT(OUT)	:: E

      ! Nice and hard-coded for now.
      ! At some point, will need to be able to choose between
      ! functions and so on.
      E = 0

    END SUBROUTINE get_E_mag

    !----------------------------------------------------
    !> Get the direction e=(ex, ey, ez) of the electric
    !> field E at position
    !----------------------------------------------------
    SUBROUTINE get_E_vec(position, e)
      
      !> Position coordinates
      REAL, DIMENSION(3), INTENT(IN)  :: position
      !> Electric field unit vector
      REAL, DIMENSION(3), INTENT(OUT) :: e

      ! Nice and hard-coded for now.
      ! At some point, will need to be able to choose between
      ! functions and so on.
      e = (/0., 0., 0./)
      
    END SUBROUTINE get_E_vec

    !----------------------------------------------------
    !> Get the full electric field vector at position
    !----------------------------------------------------
    SUBROUTINE get_E(position, E)
      
      !> Position coordinates
      REAL, DIMENSION(3), INTENT(IN)  :: position
      !> Electric field vector
      REAL, DIMENSION(3), INTENT(OUT) :: E
      !> Electric field unit vector
      REAL, DIMENSION(3)	      :: e_vec
      !> Electric field magnitude
      REAL			      :: E_mag

      CALL get_E_mag(position, E_mag)
      CALL get_E_vec(position, e_vec)

      E = E_mag*e_vec
      
    END SUBROUTINE get_E


END MODULE fields
