      REAL FUNCTION SCULIB_JNU (NU, T, STATUS)
*+
*  Name:
*     SCULIB_JNU

*  Purpose:
*     Calculate the Rayleigh-Jeans corrected brightness temperature

*  Description:
*     This function calculates the Rayleigh-Jeans corrected brightness
*     temperature of radiation at frequency NU and temperature T.
*
*     JNU =    X * T           where  X = h * NU
*          .----------                    ------
*           exp(X) - 1                    k * T
*
*     If the absolute value of X is less than 1e-4 JNU = T, or if the absolute
*     value of X is greater than 20 JNU = 0. JNU is also zero if
*     KT is close to zero (to prevent division by zero errors)

*  Invocation:
*     JNU = SCULIB_JNU (NU, T, STATUS)

*  Arguments:
*     NU                = REAL (Given)
*           frequency (Hz)
*     T                 = REAL (Given)
*           temperature (K)
*     STATUS            = INTEGER (Given and returned)
*           global status

*  Returned Value:
*     SCULIB_JNU = REAL
*        Rayleigh-Jeans corrected brightness temperature

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     7-OCT-1993: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      REAL NU
      REAL T

*  Arguments Given & Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:
      REAL H                                ! Planck
      PARAMETER (H = 6.6252E-27)
      REAL K                                ! Boltzmann
      PARAMETER (K = 1.38046E-16)

*  Local variables:
      REAL X
      REAL KT

*  Internal References:

*  Local data:

*.

*     Set to bad value in case we return with bad status
      SCULIB_JNU = VAL__BADR

*     Now we can return safely if status is bad
      IF (STATUS .NE. SAI__OK) RETURN

*     Calculate denominator
      KT = K * T

*     If denominator is almost zero (< 1E-99) then JNU is 0
      IF (KT .LT. 1E-32) THEN
         SCULIB_JNU = 0.0
         RETURN
      END IF

*     Calculate exponent
      X = (H * NU) / KT

      IF (ABS(X) .LT. 1.0E-4) THEN
         SCULIB_JNU = T
      ELSE IF (ABS(X) .GT. 20.0) THEN
         SCULIB_JNU = 0.0
      ELSE
         SCULIB_JNU = X * T / (EXP(X) - 1.0)
      END IF

      END
