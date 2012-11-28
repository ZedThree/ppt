!----------------------------------------------------
!> This module handles the electromagnetic fields.
!----------------------------------------------------
MODULE fields

  USE dipole
  USE globals

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
      
      B = B_mag

      ! B = 1.
!      B = position(3)

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

      b = b_vec

      ! b = (/0., 0., 1./)
!      b = (/-0.5*position(1), -0.5*position(2), 1.*position(3)/)
      
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

      !> Temporary values for the magnetic field
      REAL, DIMENSION(3) :: b_temp1, b_temp2
      !> Particle's position relative to dipoles
      REAL, DIMENSION(3) :: pos_temp1, pos_temp2
      
      SELECT CASE(B_type)
      CASE(1)
         CALL get_B_mag(position, B_mag)
         CALL get_B_vec(position, b_vec)

         B = B_mag*b_vec
      CASE(2)
         ! Get particle's positive relative to each dipole
         pos_temp1 = position - dipole1%position
         pos_temp2 = position - dipole2%position

         ! PRINT*, "pos1: ", pos_temp1
         ! PRINT*, "pos2: ", pos_temp2

         CALL dipole_field(pos_temp1, dipole1, b_temp1)
         CALL dipole_field(pos_temp2, dipole2, b_temp2)

         B = b_temp1 ! + b_temp2
         ! PRINT*, "B1: ", b_temp1
         ! PRINT*, "B2: ", b_temp2
         
      END SELECT
      
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
      
      E = E_mag

      ! E = 1.

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

      e = e_vec

      ! e = (/1., 0., 0./)
      
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
