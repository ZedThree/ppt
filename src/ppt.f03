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
  USE particle
  USE fields
  USE pusher
  ! USE writer

  IMPLICIT none

  !> Magnitude of B
  REAL :: b_mag
  !> Direction of B
  REAL, DIMENSION(3) :: b_vec

  ! read input file

  CALL init_particle

  CALL get_B_mag(position, b_mag)
  CALL get_B_vec(position, b_vec)
  
  PRINT*, "position: ", position
  PRINT*, "velocity: ", velocity
  PRINT*, "B: ", b_mag*b_vec
  PRINT*, "v x B: ", cross(velocity, b_mag*b_vec)
  
  ! push particle

  ! write output

END PROGRAM ppt
