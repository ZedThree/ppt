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
      
      !> Spatial coordinates
      position(X_coord) = 1
      position(Y_coord) = 1
      position(Z_coord) = 0
      !> Velocity coordinates
      velocity(X_coord) = 1
      velocity(Y_coord) = 0
      velocity(Z_coord) = 1

    END SUBROUTINE init_particle

END MODULE particle
