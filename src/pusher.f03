!----------------------------------------------------
!> This module solves the equations of motion.
!----------------------------------------------------
MODULE pusher

  USE fields

  IMPLICIT none

  CONTAINS
  ! get the particle's properties

  ! get the fields at particle location

  ! calculate forces and so on

  ! update particle properties

    !----------------------------------------------------
    !> Computes the cross product of two vectors, A and B
    !----------------------------------------------------
    FUNCTION cross(a, b)
      
      !> The cross product of A and B
      REAL, DIMENSION(3) :: cross
      !> Input vectors
      REAL, DIMENSION(3) :: a, b

      cross(1) = a(2)*b(3) - a(3)*b(2)
      cross(2) = a(3)*b(1) - a(1)*b(3)
      cross(3) = a(1)*b(2) - a(2)*b(1)

    END FUNCTION cross

    !----------------------------------------------------
    !> Runge-Kutta fourth order integrator
    !----------------------------------------------------
    SUBROUTINE rk4(position, velocity, dt)

      !> Initial position, returns as final position
      REAL, INTENT(INOUT) :: position
      !> Initial velocity, returns as final velocity
      REAL, INTENT(INOUT) :: velocity
      REAL, INTENT(OUT) :: yout
      ! dummy variables
      REAL :: h, f1, f2, f3, f4
      h = deltas
      f1 = h*grad_pot(x)
      f2 = h*grad_pot(x + h/2.0)
      f3 = h*grad_pot(x + h/2.0)
      f4 = h*grad_pot(x + h)
      yout = y + (f1 + 2*f2  +2*f3 + f4)/6.0
    END SUBROUTINE rk4


END MODULE pusher
