!----------------------------------------------------
!> This module contains all the global variables
!----------------------------------------------------
MODULE globals

  IMPLICIT none
  
  ! Particle properties
  TYPE particle_obj
     !> Particle spatial coordinates
     REAL, DIMENSION(3) :: position
     !> Particle velocity coordinates
     REAL, DIMENSION(3) :: velocity
     !> Particle mass
     REAL :: mass = 1.
     !> Particle charge
     REAL :: charge = 1.
  END TYPE particle_obj
  
  !> The particle
  TYPE(particle_obj) :: particle1

  !> X coordinate index
  INTEGER :: X_coord = 1
  !> Y coordinate index
  INTEGER :: Y_coord = 2
  !> Z coordinate index
  INTEGER :: Z_coord = 3

  ! Magnetic field properties
  !> Field type: 1) constant vector, 2) two dipoles
  INTEGER :: B_type
  !> Field vector
  REAL, DIMENSION(3) :: b_vec = (/ 0., 0., 1./)
  !> Field strength
  REAL 		     :: B_mag = 1.

  ! Electric field properties
  !> Field type: 1) constant vector, 2) two dipoles
  INTEGER :: E_type
  !> Field vector
  REAL, DIMENSION(3) :: e_vec = (/ 1., 0., 0./)
  !> Field strength
  REAL 		     :: E_mag = 1.

  ! Dipole properties
  !> distance between dipoles
  REAL :: dipole_sep
  !> size of dipole magnetic moments
  REAL :: dipole_size

  !> A dipole
  TYPE dipole_obj
     !> position of dipole centre
     REAL, DIMENSION(3) :: position
     !> magnitude of dipole
     REAL :: size
  END TYPE dipole_obj
 
  !> the two dipoles
  TYPE(dipole_obj) :: dipole1, dipole2

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


