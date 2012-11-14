!----------------------------------------------------
!> This module deals with the particle's properties
!----------------------------------------------------
MODULE particle

  USE globals

  IMPLICIT none

  CONTAINS

    !----------------------------------------------------
    !> Initialise the particle's properties.
    !> Currently very boring
    !----------------------------------------------------
    SUBROUTINE init_particle
      
      ! Spatial coordinates
      position(X_coord) = 1
      position(Y_coord) = 1
      position(Z_coord) = 0
      ! Velocity coordinates
      velocity(X_coord) = 1
      velocity(Y_coord) = 0
      velocity(Z_coord) = 1

    END SUBROUTINE init_particle

    !----------------------------------------------------
    !> Calculate the particle's kinetic energy
    !----------------------------------------------------
    REAL FUNCTION kinetic_energy(velocity)

      !> Velocity of particle
      REAL, DIMENSION(3), INTENT(IN) :: velocity
      !> Loop index
      INTEGER :: coord
      !> Temporary variable
      REAL :: temp
     
      temp = 0.

      ! Sum the squares of the velocity components
      DO coord = 1, 3
         temp = temp + velocity(coord)**2
      END DO
      
      ! K.E. = 1/2 * m *v^2
      kinetic_energy = 0.5*mass*temp

    END FUNCTION kinetic_energy

END MODULE particle
