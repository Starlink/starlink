      SUBROUTINE SCULIB_FIT_MULT (N, ALPHA, BETA, GAMMA, STATUS)
*+
*  Name:
*     SCULIB_FIT_MULT

*  Purpose:
*     matrix vector multiplication routine

*  Description:
*     If status is good on entry, this routine will multiply the N-vector
*     BETA by the NxN matrix ALPHA to give the N-vector GAMMA. GAMMA should
*     not be the same as BETA as this would mean BETA being overwritten
*     during the multiplication process giving an incorrect answer.

*  Invocation:
*     CALL SCULIB_FIT_MULT (N, ALPHA, BETA, GAMMA, STATUS)

*  Arguments:
*     N                      = INTEGER (Given)
*           dimensions of square matrix
*     ALPHA (N,N)            = DOUBLE PRECISION (Given)
*           the multiplying matrix
*     BETA (N)               = DOUBLE PRECISION (Given)
*           the multiplied vector
*     GAMMA (N)              = DOUBLE PRECISION (Returned)
*           the result vector (GAMMA and BETA should not be the same array)
*     STATUS                 = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (JFL@ROE.AC.UK)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     28-MAR-1995: original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION ALPHA (N,N)
      DOUBLE PRECISION BETA (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION GAMMA (N)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I                        ! DO loop index
      INTEGER J                        ! DO loop index

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO J = 1, N
         GAMMA (J) = 0.0D0
         DO I = 1, N
            GAMMA (J) = GAMMA (J) + ALPHA (I,J) * BETA (I)
         END DO
      END DO

      END
