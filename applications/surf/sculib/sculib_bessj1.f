      FUNCTION SCULIB_BESSJ1 (X, STATUS)
*+
*  Name:
*     SCULIB_BESSJ1

*  Purpose:
*     calculates Bessel function J1(x)

*  Description:
*     If status is good on entry this function returns the Bessel function
*     J1(x) for any real x.

*  Invocation:
*     Y = SCULIB_BESSJ1 (X, STATUS)

*  Arguments:
*     X                      = REAL (Given)
*           the argument of the J1 Bessel function
*     STATUS                 = INTEGER (Given and returned)
*           global status

*  Returned Value:
*     SCULIB_BESSJ1 = REAL
*       Bessel function J1(X)

*  Notes:
*     Probably should be changed to use PDA_DBESJ1 (SUN/194). That routine
*     is DOUBLE PRECISION.

*  Method:
*     Uses an algorithm from Numerical Recipes in Fortran.

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     20-AUG-1995: original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      REAL X

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL SCULIB_BESSJ1

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      REAL AX
      REAL XX
      REAL Z
      DOUBLE PRECISION P1, P2, P3, P4, P5
      DOUBLE PRECISION Q1, Q2, Q3, Q4, Q5
      DOUBLE PRECISION R1, R2, R3, R4, R5, R6
      DOUBLE PRECISION S1, S2, S3, S4, S5, S6
      DOUBLE PRECISION Y
      SAVE P1, P2, P3, P4, P5
      SAVE Q1, Q2, Q3, Q4, Q5
      SAVE R1, R2, R3, R4, R5, R6
      SAVE S1, S2, S3, S4, S5, S6

*  Internal References:

*  Local data:
      DATA R1, R2, R3, R4, R5, R6 /
     :  72362614232.0d0, -7895059235.0d0, 242396853.1d0,
     :  -2972611.439d0, 15704.48260d0, -30.16036606d0/,
     :  S1, S2, S3, S4, S5, S6 /
     :  144725228442.0D0, 2300535178.0D0, 18583304.74D0,
     :  99447.43394D0, 376.9991397D0, 1.0D0/
      DATA P1, P2, P3, P4, P5 /
     :  1.0D0, 0.183105D-2, -0.3516396496D-4,
     :  0.2457520174D-5, -0.240337019D-6/,
     :  Q1, Q2, Q3, Q4, Q5 /
     :  0.04687499995D0, -0.2002690873D-3, 0.8449199096D-5,
     :  -0.88228987D-6,0.105787412D-6/

*.

*     Set to bad in case status is bad
*     Otherwise we would be undefined on return
      SCULIB_BESSJ1 = VAL__BADR

      IF (STATUS .NE. SAI__OK) RETURN

      IF (ABS(X) .LT. 8.0) THEN
         Y = X**2
         SCULIB_BESSJ1 = X*(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6))))) /
     :     (S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
      ELSE
         AX = ABS(X)
         Z = 8.0 / AX
         Y = Z**2
         XX = AX - 2.356194491
         SCULIB_BESSJ1 = SQRT (0.636619772/AX)*(COS(XX)*(P1+Y*(P2+Y*
     :     (P3+Y*(P4+Y*P5))))-Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*
     :     (Q4+Y*Q5)))))*SIGN(1.0,X)
      END IF

      END
