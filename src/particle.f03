!----------------------------------------------------
!> This module deals with the particle's properties
!----------------------------------------------------
MODULE particle

  USE globals
  USE normalisations

  IMPLICIT none

  CONTAINS

    !----------------------------------------------------
    !> Initialise the particle's properties.
    !> Currently very boring
    !----------------------------------------------------
    SUBROUTINE init_particle(particle, position, velocity, mass, charge)
      
      TYPE(particle_obj), INTENT(inout) :: particle
      !> Particle temporary variables
      REAL, DIMENSION(3), INTENT(in) :: position, velocity
      REAL, INTENT(in)		     :: mass, charge


      particle1%position = position
      particle1%velocity = velocity
      particle1%mass     = mass * MassProton
      particle1%charge   = charge * ECharge

      ! ! Spatial coordinates
      ! position(X_coord) = 1
      ! position(Y_coord) = 1
      ! position(Z_coord) = 0
      ! ! Velocity coordinates
      ! velocity(X_coord) = 1
      ! velocity(Y_coord) = 0
      ! velocity(Z_coord) = 1

    END SUBROUTINE init_particle

    !----------------------------------------------------
    !> Calculate the particle's kinetic energy
    !----------------------------------------------------
    REAL FUNCTION kinetic_energy(particle)

      !> particle
      TYPE(particle_obj), INTENT(in) :: particle
      ! !> Velocity of particle
      ! REAL, DIMENSION(3), INTENT(IN) :: velocity
      !> Loop index
      INTEGER :: coord
      !> Temporary variable
      REAL :: temp
     
      temp = 0.

      ! Sum the squares of the velocity components
      DO coord = 1, 3
         temp = temp + particle%velocity(coord)**2
      END DO
      
      ! K.E. = 1/2 * m *v^2
      kinetic_energy = 0.5*particle%mass*temp

    END FUNCTION kinetic_energy

END MODULE particle
