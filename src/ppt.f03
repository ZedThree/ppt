!----------------------------------------------------
! Doxygen front page
!> \mainpage
!! Peter's Particle Tracker (PPT).
!! ===============================
!!
!! A little program for tracking charged particles in
!! electromagnetic fields.
!!
!! To do:
!! ------
!! 
!! - Write a reader
!! - Write a writer
!<
!----------------------------------------------------
!> Main program.
!> This is the top-most file.
!----------------------------------------------------
PROGRAM ppt

  USE globals
  USE reader
  USE particle
  USE dipole
  USE fields
  USE pusher
  USE writer

  IMPLICIT none

  !> Time
  REAL :: time
  !> Time step number
  INTEGER :: step

  ! !> Magnitude of B
  ! REAL :: b_mag
  ! !> Direction of B
  ! REAL, DIMENSION(3) :: b_vec
  !> Kinetic energy of particle
  REAL :: energy = 0.

  PRINT*, "------------------------------------"
  PRINT*, "Starting Peter's Particle Tracker"
  PRINT*, "------------------------------------"


  CALL read_input

!  CALL init_particle

  CALL init_dipole(dipole_sep, dipole_size, dipole1, dipole2)

  PRINT*, "dipole1: ", dipole1%position, dipole1%size
  PRINT*, "dipole2: ", dipole2%position, dipole2%size
  
  ! Boris needs to move velocity back in time by half a time step
  CALL boris(particle1, -0.5*dt)

  DO step = 0, nsteps
     time = step * dt
!     CALL rk4(position, velocity)
     CALL boris(particle1, dt)
!     CALL get_B(position, b_vec)
     energy = 0.
     energy = kinetic_energy(particle1)
     CALL write_output(particle1%position, particle1%velocity, energy, time)
  END DO
  
  ! push particle

  ! write output

END PROGRAM ppt
