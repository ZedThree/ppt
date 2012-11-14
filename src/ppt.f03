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
  USE fields
  USE pusher
  USE writer

  IMPLICIT none

  !> Time
  REAL :: time
  !> Time step number
  INTEGER :: step

  !> Magnitude of B
  REAL :: b_mag
  !> Direction of B
  REAL, DIMENSION(3) :: b_vec
  !> Kinetic energy of particle
  REAL :: energy = 0.

  PRINT*, "------------------------------------"
  PRINT*, "Starting Peter's Particle Tracker"
  PRINT*, "------------------------------------"

!  CALL init_particle
  CALL read_input

  ! Boris needs to move velocity back in time by half a time step
  CALL boris(position, velocity, -0.5*dt)

  DO step = 0, nsteps
     time = step * dt
!     CALL rk4(position, velocity)
     CALL boris(position, velocity, dt)
!     CALL get_B(position, b_vec)
     energy = 0.
     energy = kinetic_energy(velocity)
     CALL write_output(position, velocity, energy, time)
  END DO
  
  ! push particle

  ! write output

END PROGRAM ppt
