MODULE reader

  USE globals

  IMPLICIT NONE

CONTAINS
  
  !----------------------------------------------------
  !> Read an input file
  !----------------------------------------------------
  SUBROUTINE read_input !(filename)

    !> Name of the input file
!    CHARACTER(LEN=20), INTENT(in) :: filename

    NAMELIST /stepper/ nsteps, dt
    NAMELIST /particle/ position, velocity, mass, charge

    PRINT*, "------------------------------"
    PRINT*, "Reading input file: input_file "

    OPEN(input_dat, FILE="input_file", STATUS='OLD')

    READ(input_dat, NML=stepper)
    WRITE(*,nml=stepper)
    
    READ(input_dat, NML=particle)
    WRITE(*,nml=particle)

    CLOSE(input_dat)
    PRINT*, "------------------------------"

  END SUBROUTINE read_input

END MODULE reader
