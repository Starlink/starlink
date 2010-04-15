      SUBROUTINE SCULIB_FIT_D2XISQ_DAJ2 (XISQ_ROUTINE, N, A, J,
     :  D2XISQ_DAJ2, STATUS)
*+
*  Name:
*     SCULIB_FIT_D2XISQ_DAJ2

*  Purpose:
*     calculate second differential of chi-squared
*     with respect to a fit parameter

*  Description:
*     If status is good on entry this routine will calculate the 2nd order
*     differential of chi-squared with respect to A(J). It does this
*     numerically by calling the supplied routine XISQ_ROUTINE to calculate
*     chi-squared at the current parameter values, then again at A(J)+DELTA_AJ
*     and A(J)-DELTA_AJ, then using the formula:-
*
*        D2XISQ_DAJ2 = XISQ (AJ+DELTA_AJ) - 2 * XISQ (AJ) + XISQ (AJ-DELTA_AJ)
*                     .-------------------------------------------------------
*                                         DELTA_AJ ** 2
*
*     DELTA_AJ is equal to the absolute value of 0.001 * A(J) or, if this is
*     zero, 0.001.

*  Invocation:
*     CALL SCULIB_FIT_D2XISQ_DAJ2 (XISQ_ROUTINE, N, A, J,
*    :  D2XISQ_DAJ2, STATUS)

*  Arguments:
*     XISQ_ROUTINE (XISQ, N, A, STATUS) = EXTERNAL ROUTINE (Given)
*           name of routine that will calculate chi-squared
*     N                       = INTEGER (Given)
*           the number of parameters in the fit
*     A(N)                    = DOUBLE PRECISION (Given and returned)
*           the fit parameters
*     J                       = INTEGER (Given)
*           the index of the parameter to be varied
*     D2XISQ_DAJ2             = DOUBLE PRECISION (Returned)
*           the second differential of chi-squared with respect to A(J)
*     STATUS                  = INTEGER (Given and returned)
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
*     23-MAR-1995: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION A (N)
      INTEGER J

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION D2XISQ_DAJ2

*  Status:
      INTEGER STATUS

*  External references:
      EXTERNAL XISQ_ROUTINE

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION DELTA_AJ     ! small increment in A(J)
      DOUBLE PRECISION XISQMINUS    ! value of chi-squared at A(J) - DELTA_AJ
      DOUBLE PRECISION XISQPLUS     ! value of chi-squared at A(J) + DLETA_AJ
      DOUBLE PRECISION XISQ0        ! current value of chi-squared

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  calculate the increment to A(J)

      DELTA_AJ = ABS (0.001D0 * A(J))
      IF (DELTA_AJ .EQ. 0.0D0) THEN
         DELTA_AJ = 0.001D0
      END IF

*  apply it and calculate function at 2 positions either side of A(J)

      CALL XISQ_ROUTINE (XISQ0, N, A, STATUS)

      A(J) = A(J) + DELTA_AJ
      CALL XISQ_ROUTINE (XISQPLUS, N, A, STATUS)

      A(J) = A(J) - 2.0D0 * DELTA_AJ
      CALL XISQ_ROUTINE (XISQMINUS, N, A, STATUS)

*  calculate result and go back to original A(J)

      D2XISQ_DAJ2 = (XISQPLUS - 2.0D0*XISQ0 + XISQMINUS) / DELTA_AJ**2
      A(J) = A(J) + DELTA_AJ

      END
