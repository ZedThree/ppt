!----------------------------------------------------
!> This module writes the particle's position and so
!> on to file.
!----------------------------------------------------
MODULE writer

  USE globals

  IMPLICIT none

CONTAINS
  
  SUBROUTINE write_output(position, velocity, energy, time)
    
    !> Particle position
    REAL, DIMENSION(3), INTENT(in) :: position
    !> Particle velocity
    REAL, DIMENSION(3), INTENT(in) :: velocity
    !> Particle energy
    REAL, INTENT(in)		   :: energy
    !> Time at current time-step
    REAL, INTENT(in)		   :: time

    !> Format definition for output
    

    ! open a file
    OPEN(unit=20, FILE="./src/output_file", POSITION='APPEND')
    ! write to file
    WRITE(20,'(3ES15.4, 3ES15.4, 2ES15.4)') position, velocity, energy, time
    ! close file
    CLOSE(unit=20)

  END SUBROUTINE write_output
  
END MODULE writer
