*+  AXV - Convert radians position to value in axis units
      REAL FUNCTION AXV( AX, XD )
*
*    Description :
*
*     Converts the value XD which is a radian measure for the spatial axis
*     numbered AX into the dataset units for that axis.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Jul 93 : Original. Taken from inline definition (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  AX                ! Axis number
      REAL                     XD                ! Axis value
*-

      AXV = XD/AX_TOR(AX)

      END
*+  DAT - Convert pixel number to radian offset
      REAL FUNCTION DAT( AX, IX )
*
*    Description :
*
*     Finds the radian value of the centre of the IX'th pixel in the AX'th
*     axis.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Jul 93 : Original. Taken from inline definition (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  AX                ! Axis number
      INTEGER                  IX                ! Pixel number
*-

      DAT = ( REAL(IX)-1.0 ) * AX_DR(AX) + AX_BR(AX)

      END
*+  PIX - Convert radians position to axis pixel number
      INTEGER FUNCTION PIX( AX, XD )
*
*    Description :
*
*     Converts the value XD which is a radian measure for the spatial axis
*     numbered AX into the integer pixel in which that point falls.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Jul 93 : Original. Taken from inline definition (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  AX                ! Axis number
      REAL                     XD                ! Axis value
*-

      PIX = NINT(( XD - AX_BR(AX) ) / AX_DR(AX)) + 1

      END
