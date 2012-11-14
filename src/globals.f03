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

  ! Stepper variables
  !> Length of time step
  REAL 	  :: dt = 0.01
  !> Number of time steps
  INTEGER :: nsteps = 100

  ! Misc variables
  !> Input file unit number
  INTEGER :: input_dat = 10
  !> STDIN file unit number
  INTEGER :: stdin = 6
  !> Output file unit number
  INTEGER :: output_dat = 20
  
  
END MODULE globals


