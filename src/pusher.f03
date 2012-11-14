!----------------------------------------------------
!> This module solves the equations of motion.
!----------------------------------------------------
MODULE pusher

  USE globals
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
    !> Calculate the acceleration of a particle at
    !> position
    !----------------------------------------------------
    FUNCTION acceleration(position, velocity)

      !> Acceleration due to EM fields
      REAL, DIMENSION(3) :: acceleration
      !> Position of particle
      REAL, DIMENSION(3), INTENT(IN) :: position
      !> Velocity of particle
      REAL, DIMENSION(3), INTENT(IN) :: velocity
      !> Lorentz force vector
      REAL, DIMENSION(3) :: force
      !> Magnetic field
      REAL, DIMENSION(3) :: B
      !> Electric field
      REAL, DIMENSION(3) :: E

      !> Get the EM field vectors
      CALL get_B(position, B)
      CALL get_E(position, E)

      !> Calculate Lorentz force
      force = charge*(E + cross(velocity, B))

      acceleration = force/mass
      
    END FUNCTION acceleration
    
    !----------------------------------------------------
    !> Runge-Kutta fourth order integrator
    !----------------------------------------------------
    SUBROUTINE rk4(position, velocity)

      !> Initial position, returns as final position
      REAL, DIMENSION(3), INTENT(INOUT) :: position
      !> Initial velocity, returns as final velocity
      REAL, DIMENSION(3), INTENT(INOUT) :: velocity
      ! dummy variables
      REAL, DIMENSION(3) :: x1, x2, x3, x4
      REAL, DIMENSION(3) :: v1, v2, v3, v4
      REAL, DIMENSION(3) :: a1, a2, a3, a4

      x1 = position
      v1 = velocity
      a1 = acceleration(x1, v1)

      x2 = position + 0.5*v1*dt
      v2 = velocity + 0.5*a1*dt
      a2 = acceleration(x2, v2)

      x3 = position + 0.5*v2*dt
      v3 = velocity + 0.5*a2*dt
      a3 = acceleration(x3, v3)      
      
      x4 = position + v3*dt
      v4 = velocity + a3*dt
      a4 = acceleration(x3, v3)

      position = position + (v1 + 2*v2 + 2*v3 + v4)*(dt/6.)
      velocity = velocity + (a1 + 2*a2 + 2*a3 + a4)*(dt/6.)

    END SUBROUTINE rk4

    !----------------------------------------------------
    !> Boris integrator.
    !> See that PIC website for more details.
    !> Or the Boris paper?
    !----------------------------------------------------
    SUBROUTINE boris(position, velocity, dt)
      
      !> Initial position, returns as final position
      REAL, DIMENSION(3), INTENT(INOUT) :: position
      !> Initial velocity, returns as final velocity
      REAL, DIMENSION(3), INTENT(INOUT) :: velocity
      !> Time-step
      REAL , INTENT(in) :: dt
      
      ! dummy variables
      !> v_minus
      REAL, DIMENSION(3) :: v_minus
      !> v_plus
      REAL, DIMENSION(3) :: v_plus
      !> v_prime
      REAL, DIMENSION(3) :: v_prime
      !> t vector
      REAL, DIMENSION(3) :: t
      !> s vector
      REAL, dimension(3) :: s
      !> Magnetic field
      REAL, DIMENSION(3) :: B
      !> Electric field
      REAL, DIMENSION(3) :: E
      !> t magnitude, squared
      REAL :: t_mag2

      CALL get_B(position, B)
      CALL get_E(position, E)

      t = (charge/mass)*B*0.5*dt
      t_mag2 = t(1)*t(1) + t(2)*t(2) + t(3)*t(3)

      s = 2*t/(1+t_mag2)

      v_minus = velocity + (charge/mass)*E*0.5*dt
      v_prime = v_minus + cross(v_minus, t)
      v_plus  = v_minus + cross(v_prime, s)

      velocity = v_plus + (charge/mass)*E*0.5*dt

      IF (dt .LT. 0.) RETURN

      position = position + velocity*dt

    END SUBROUTINE boris
    
END MODULE pusher
