!----------------------------------------------------
!> This module reads the input files
!----------------------------------------------------
MODULE reader

  USE globals
  USE particle

  IMPLICIT NONE

CONTAINS
  
  !----------------------------------------------------
  !> Read an input file
  !----------------------------------------------------
  SUBROUTINE read_input !(filename)

    !> Particle temporary variables
    REAL, DIMENSION(3) :: position, velocity
    REAL	       :: mass, charge

    !> Name of the input file
!    CHARACTER(LEN=20), INTENT(in) :: filename

    NAMELIST /stepper/ nsteps, dt
    NAMELIST /particle/ position, velocity, mass, charge
    NAMELIST /Bfield/ B_type, b_vec, B_mag, dipole_sep, dipole_size
    NAMELIST /Efield/ E_type, e_vec, E_mag

    PRINT*, "------------------------------"
    PRINT*, "Reading input file: input_file "

    OPEN(input_dat, FILE="input_file", STATUS='OLD')

    READ(input_dat, NML=stepper)
    WRITE(*,nml=stepper)
    
    READ(input_dat, NML=particle)
    WRITE(*,nml=particle)

    CALL init_particle(particle1, position, velocity, mass, charge)

    READ(input_dat, NML=Bfield)
    WRITE(*,nml=Bfield)

    READ(input_dat, NML=Efield)
    WRITE(*,nml=Efield)

    CLOSE(input_dat)
    PRINT*, "------------------------------"

  END SUBROUTINE read_input

END MODULE reader
