!--------------------------------------------------------------
!> This module contains various physical parameters in SI units
!--------------------------------------------------------------
MODULE normalisations

  IMPLICIT NONE

  !> Pi
  REAL, PARAMETER :: Pi           = 3.141592653589793238462643383279502884197
  !> Permittivity of free space
  REAL, PARAMETER :: Epsilon0     = 8.8541878E-12
  !> Permeability of free space
  REAL, PARAMETER :: Mu0          = 4*Pi*1e-7
  !> Proton mass
  REAL, PARAMETER :: MassProton   = 1.6726231E-27
  !> Electron mass
  REAL, PARAMETER :: MassElectron = 9.1094E-31
  !> Elementary charge
  REAL, PARAMETER :: ECharge      = 1.6021765E-19
  !> Boltzmann constant
  REAL, PARAMETER :: KBoltz       = 1.3806E-23
  !> Speed of light
  REAL, PARAMETER :: CLight       = 2.9979E8

END MODULE normalisations
