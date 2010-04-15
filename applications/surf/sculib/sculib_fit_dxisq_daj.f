      SUBROUTINE SCULIB_FIT_DXISQ_DAJ (XISQ_ROUTINE, N, A, J, DXISQ_DAJ,
     :  STATUS)
*+
*  Name:
*     SCULIB_FIT_DXISQ_DAJ

*  Purpose:
*     calculate gradient in chi-squared with `a'

*  Description:
*     If status is good on entry this routine will calculate the gradient in
*     the chi-squared of the fit with variation in fit parameter J. It does
*     this numerically by calling the supplied routine XISQ_ROUTINE to
*     calculate chi-squared for (A(J)+DELTA_AJ) and (A(J)-DELTA_AJ), then
*     using the equation:-
*
*       DXISQ_DAJ = XISQ (A(J)+DELTA_AJ) - XISQ (A(J)-DELTA_AJ)
*                   .-------------------------------------------
*                                2.0 * DELTA_AJ
*
*     DELTA_AJ is equal to the absolute value of 0.001 * A(J) or, if this is
*     zero, 0.001.

*  Invocation:
*     CALL SCULIB_FIT_DXISQ_DAJ (XISQ_ROUTINE, N, A, J, DXISQ_DAJ,
*    :  STATUS)

*  Arguments:
*     XISQ_ROUTINE (XISQ, N, A, STATUS) = EXTERNAL ROUTINE (Given)
*           name of routine that will calculate xi-squared
*     N                       = INTEGER (Given)
*           the number of parameters in the fit
*     A (N)                   = DOUBLE PRECISION (Given and returned)
*           the fit parameters
*     J                       = INTEGER (Given)
*           the index of the parameter to be varied
*     DXISQ_DAJ               = DOUBLE PRECISION (Returned)
*           the gradient of chi-squared with A(J)
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
      DOUBLE PRECISION DXISQ_DAJ

*  Status:
      INTEGER STATUS

*  External references:
      EXTERNAL XISQ_ROUTINE

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION DELTA_AJ      ! small increment in A(J)
      DOUBLE PRECISION XISQP         ! value of chi-squared at A(J) + DELTA_AJ
      DOUBLE PRECISION XISQM         ! value of chi-squared at A(J) - DELTA_AJ

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  calculate increment in A(J)

      DELTA_AJ = ABS (0.001D0 * A(J))
      IF (DELTA_AJ .EQ. 0.0D0) THEN
         DELTA_AJ = 0.001D0
      END IF

*  calculate chi-squared above and below current position

      A(J) = A(J) - DELTA_AJ
      CALL XISQ_ROUTINE (XISQM, N, A, STATUS)

      A(J) = A(J) + 2.0D0 * DELTA_AJ
      CALL XISQ_ROUTINE (XISQP, N, A, STATUS)

*  calculate the differential and go back to original A(J)

      DXISQ_DAJ = (XISQP - XISQM) / (2.0D0 * DELTA_AJ)
      A(J) = A(J) - DELTA_AJ

      END
