      SUBROUTINE SCULIB_FIT_MAKEALPHA (XISQ_ROUTINE, N, A, ALPHA,
     :  STATUS)
*+
*  Name:
*     SCULIB_FIT_MAKEALPHA

*  Purpose:
*     calculates alpha matrix for SCULIB_FIT_FUNCTION

*  Description:
*     This routine calculates the elements of the alpha matrix for non-linear
*     fitting as described in `Data Reduction and Error Analysis for the
*     Physical Sciences' by Bevington and Robinson, section 8.6 `the Marquardt
*     Method'.
*
*        The routine does this by calling routines SCULIB_FIT_D2XISQ_DAJ2
*     and SCULIB_FIT_D2XISQ_DAJK for each element required. The routine will
*     only execute if entered with good status.

*  Invocation:
*     CALL SCULIB_FIT_MAKEALPHA (XISQ_ROUTINE, N, A, ALPHA, STATUS)

*  Arguments:
*     XISQ_ROUTINE (XISQ, N, A, STATUS) = EXTERNAL ROUTINE (Given)
*           routine to calculate chi-squared of current fit
*     N                         = INTEGER (Given)
*           the number of fit parameters
*     A (N)                     = DOUBLE PRECISION (Given)
*           the values of the fit parameters
*     ALPHA (N,N)               = DOUBLE PRECISION (Returned)
*           the alpha matrix
*     STATUS                    = INTEGER (Given and returned)
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
*     27-MAR-1995: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION A (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION ALPHA (N,N)

*  Status:
      INTEGER STATUS

*  External references:
      EXTERNAL XISQ_ROUTINE

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION D2XISQ_DAJK ! differential in chi-squared with variations
                                   ! in A(J) and A(K)
      DOUBLE PRECISION D2XISQ_DAJ2 ! 2nd differential in chi-squared with
                                   ! variation in A(J)
      INTEGER J                    ! DO loop variable
      INTEGER K                    ! DO loop variable

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO J = 1, N

*  calculate diagonal value, then off-diagonals depending on its value

         CALL SCULIB_FIT_D2XISQ_DAJ2 (XISQ_ROUTINE, N, A, J,
     :     D2XISQ_DAJ2, STATUS)
         ALPHA (J,J) = 0.5D0 * D2XISQ_DAJ2

         IF (ALPHA(J,J) .EQ. 0.0D0) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETI ('J', J)
            CALL ERR_REP (' ', 'SCULIB_FIT_MAKEALPHA: diagonal '//
     :        'element is zero - J = ^J', STATUS)
            GOTO 100

         ELSE IF (ALPHA(J,J) .LT. 0.0D0) THEN

            ALPHA (J,J) = -ALPHA (J,J)

            IF (J .GT. 1) THEN
               DO K = 1, J-1
                  ALPHA (J,K) = 0.0D0
                  ALPHA (K,J) = 0.0D0
               END DO
            END IF

         ELSE

            IF (J .GT. 1) THEN
               DO K = 1, J-1
                  CALL SCULIB_FIT_D2XISQ_DAJK (XISQ_ROUTINE, N, A, J, K,
     :              D2XISQ_DAJK, STATUS)
                  ALPHA (J,K) = 0.5D0 * D2XISQ_DAJK
                  ALPHA (K,J) = ALPHA (J,K)
               END DO
            END IF
         END IF

      END DO

*  exit on error

 100  CONTINUE

      END
