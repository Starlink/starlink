      SUBROUTINE SCULIB_FIT_D2XISQ_DAJK (XISQ_ROUTINE, N, A, J, K,
     :  D2XISQ_DAJK, STATUS)
*+
*  Name:
*     SCULIB_FIT_D2XISQ_DAJK

*  Purpose:
*     calculate differential of chi-squared with respect
*     to fit parameters J and K

*  Description:
*     If status is good on entry this routine will calculate the differential
*     of chi-squared with respect to A(J) and A(K). It does this numerically
*     by calling the supplied routine XISQ_ROUTINE to calculate chi-squared at
*     [A(J)+DELTA_AJ,A(K)+DELTA_AK], [A(J)-DELTA_AJ,A(K)+DELTA_AK],
*     [A(J)+DELTA_AJ,A(K)-DELTA_AK] and [A(J)-DELTA_AJ,A(K)-DELTA_AK]. The
*     result is then calculated using the formula:-
*
*      D2XISQ_DAJK = XISQ (AJ+DELTA_AJ,AK+DELTA_AK) - XISQ (AJ-DELTA_AJ,AK+DELTA_AK)
*          _    -XISQ (AJ-DELTA_AJ,AK+DELTA_AK) + XISQ (AJ-DELTA_AJ,AK-DELTA_AK)
*                .---------------------------------------------------------------
*                                    4 * DELTA_AJ * DELTA_AK
*
*     DELTA_AJ is equal to the absolute value of 0.001 * A(J) or, if this is
*     zero, 0.001. DELTA_AK is calculated in a similar way.

*  Invocation:
*     CALL SCULIB_FIT_D2XISQ_DAJK (XISQ_ROUTINE, N, A, J, K,
*    :  D2XISQ_DAJK, STATUS)

*  Arguments:
*     XISQ_ROUTINE (XISQ, N, A, STATUS) = EXTERNAL ROUTINE (Given)
*           name of routine that will calculate xi-squared
*     N                       = INTEGER (Given)
*           the number of parameters in the fit
*     A(N)                    = DOUBLE PRECISION (Given and returned)
*           the fit parameters
*     J                       = INTEGER (Given)
*           the index of the first parameter to be varied
*     K                       = INTEGER (Given)
*           the index of the second parameter to be varied
*     D2XISQ_DAJK             = DOUBLE PRECISION (Returned)
*           the differential of chi-squared with respect to A(J) and A(K)
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
      INTEGER K

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION D2XISQ_DAJK

*  Status:
      INTEGER STATUS

*  External references:
      EXTERNAL XISQ_ROUTINE

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION DELTA_AJ  ! small increment in A(J)
      DOUBLE PRECISION DELTA_AK  ! small increment in A(K)
      DOUBLE PRECISION XISQMM    ! value of chi-squared at A(J)-DELTA_AJ,
                                 ! A(K)-DELTA_AK
      DOUBLE PRECISION XISQMP    ! value of chi-squared at A(J)-DELTA_AJ,
                                 ! A(K)+DELTA_AK
      DOUBLE PRECISION XISQPM    ! value of chi-squared at A(J)+DELTA_AJ,
                                 ! A(K)-DELTA_AK
      DOUBLE PRECISION XISQPP    ! value of chi-squared at A(J)+DELTA_AJ,
                                 ! A(K)+DELTA_AK

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  calculate the increments to A(J) and A(K)

      DELTA_AJ = ABS (0.001D0 * A(J))
      IF (DELTA_AJ .EQ. 0.0D0) THEN
         DELTA_AJ = 0.001D0
      END IF

      DELTA_AK = ABS (0.001D0 * A(K))
      IF (DELTA_AK .EQ. 0.0D0) THEN
         DELTA_AK = 0.001D0
      END IF

*  apply it and calculate function at 4 positions either side of A(J),A(K)

      A(J) = A(J) + DELTA_AJ
      A(K) = A(K) + DELTA_AK
      CALL XISQ_ROUTINE (XISQPP, N, A, STATUS)

      A(K) = A(K) - 2.0D0 * DELTA_AK
      CALL XISQ_ROUTINE (XISQPM, N, A, STATUS)

      A(J) = A(J) - 2.0D0 * DELTA_AJ
      CALL XISQ_ROUTINE (XISQMM, N, A, STATUS)

      A(K) = A(K) + 2.0D0 * DELTA_AK
      CALL XISQ_ROUTINE (XISQMP, N, A, STATUS)

*  calculate result and go back to original A(J),A(K)

      D2XISQ_DAJK = (XISQPP - XISQMP - XISQPM + XISQMM) /
     :  (4.0D0 * DELTA_AJ * DELTA_AK)
      A(J) = A(J) + DELTA_AJ
      A(K) = A(K) - DELTA_AK

      END
