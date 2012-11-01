!----------------------------------------------------
!> This module contains all the global variables
!----------------------------------------------------
MODULE globals

  IMPLICIT none
  
  ! Particle properties
  !> Particle spatial coordinates
  REAL, DIMENSION(3) :: position
  !> Particle velocity coordinates
  REAL, DIMENSION(3) :: velocity
  !> X coordinate index
  INTEGER :: X_coord = 1
  !> Y coordinate index
  INTEGER :: Y_coord = 2
  !> Z coordinate index
  INTEGER :: Z_coord = 3
  !> Particle mass
  REAL :: mass = 1.
  !> Particle charge
  REAL :: charge = 1.

  !> Time step
  REAL :: dt = 1.
  
  
END MODULE globals


