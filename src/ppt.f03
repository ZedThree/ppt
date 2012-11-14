!----------------------------------------------------
! Doxygen front page
!> \mainpage
!! Peter's Particle Tracker (PPT)
!! A little program for tracking charged particles in
!! electromagnetic fields.
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

  ! read input file

  CALL init_particle

  PRINT*, "Start"
  PRINT*, "Position: ", position
  PRINT*, "Velocity: ", velocity
  PRINT*, "------------------------------------"

  DO step = 0, 20
     time = step * dt
     CALL rk4(position, velocity)
     CALL get_B(position, b_vec)
     PRINT*, "Time: ", time
     PRINT*, "Position: ", position
     PRINT*, "Velocity: ", velocity
     PRINT*, "v x B: ", cross(velocity, b_vec)
     PRINT*, "Acceleration: ", acceleration(position, velocity)
     PRINT*, "------------------------------------"
  END DO
  
  ! push particle

  ! write output

END PROGRAM ppt
