*+  SCULIB_JNU - Rayleigh-Jeans corrected brightness temperature
      REAL FUNCTION SCULIB_JNU (NU, T, STATUS)
*    Description :
*     This function calculates the Rayleigh-Jeans corrected brightness
*     temperature of radiation at frequency NU and temperature T.
*
*     JNU =    X * T           where  X = h * NU
*           ----------                    ------
*           exp(X) - 1                    k * T
*
*     If the absolute value of X is less than 1e-4 JNU = T, or if the absolute
*     value of X is greater than 20 JNU = 0.
*    Invocation :
*     JNU = SCULIB_JNU (NU, T, STATUS)
*    Parameters :
*     NU                = REAL (Given)
*           frequency (Hz)
*     T                 = REAL (Given)
*           temperature (K)
*     STATUS            = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*     7-OCT-1993: Original version 
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      REAL NU
      REAL T
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
      REAL H                                ! Planck
      PARAMETER (H = 6.6252E-27)
      REAL K                                ! Boltzmann
      PARAMETER (K = 1.38046E-16)
*    Local variables :
      REAL X
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

      X = (H * NU) / (K * T)

      IF (ABS(X) .LT. 1.0E-4) THEN
         SCULIB_JNU = T
      ELSE IF (ABS(X) .GT. 20.0) THEN
         SCULIB_JNU = 0.0
      ELSE
         SCULIB_JNU = X * T / (EXP(X) - 1.0)
      END IF

      END
