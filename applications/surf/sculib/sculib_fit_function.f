      SUBROUTINE SCULIB_FIT_FUNCTION (XISQ_ROUTINE, CHICUT, N, A,
     :  LAMBDA, ALPHA, BETA, IK, JK, DA, STATUS)
*+
*  Name:
*     SCULIB_FIT_FUNCTION

*  Purpose:
*     routine to fit a general function to data

*  Description:
*     This routine fits a general function to a data-set by searching the
*     chi-squared plane. Chi-squared at each test fit position will be
*     evaluated by the external routine XISQ_ROUTINE. The data to be fit
*     are passed to the external routine through common; see
*     SCULIB_GAUSSIAN_XISQ for an example. Each call to the routine will
*     generate one iteration of the search. On exit the fit should have an
*     improved chi-squared that should be checked for convergence (i.e. has
*     chi-squared changed by less than CHICUT between 2 iterations) before
*     the routine is called again to perform the next iteration. It is
*     recommended that the routine be entered for the first iteration with
*     LAMBDA set to 0.001. Thereafter, the routine will return the value
*     of LAMBDA to be used in the next iteration.
*
*       The search method used is that due to Marquardt as described in
*     chapter 8 of `Data Reduction and Error Analysis' by Bevington and
*     Robinson. The code is an adaptation of the Pascal routine Marquardt
*     listed in that book.
*
*       If the routine is entered with LAMBDA = 0 then it will return the
*     `error' matrix for the given fit parameters.
*


*  Invocation:
*     CALL SCULIB_FIT_FUNCTION (XISQ_ROUTINE, CHICUT, N, A,
*    :  LAMBDA, ALPHA, BETA, IK, JK, DA, STATUS)

*  Arguments:
*     XISQ_ROUTINE (XISQ, N, A, STATUS) = EXTERNAL ROUTINE (Given)
*           routine to calculate chi-squared of fit
*     CHICUT                  = DOUBLE PRECISION (Given)
*           increase in chi-squared that will make routine search with
*           larger LAMBDA
*     N                       = INTEGER (Given)
*           number of fitted parameters
*     A (N)                   = DOUBLE PRECISION (Given and returned)
*           the fitted parameters
*     LAMBDA                  = DOUBLE PRECISION (Given and returned)
*           curvature factor for alpha matrix
*     ALPHA (N,N)             = DOUBLE PRECISION (Scratch)
*           storage for alpha array
*     BETA (N)                = DOUBLE PRECISION (Scratch)
*           storage for beta array
*     IK (N)                  = INTEGER (Scratch)
*           used for inverting matrix
*     JK (N)                  = INTEGER (Scratch)
*           used for inverting matrix
*     DA (N)                  = DOUBLE PRECISION (Scratch)
*           increment in A
*     STATUS                  = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (JFL@ROE.AC.UK)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:
*     If status is good on entry the routine will enter a loop
*
*       -   SCULIB_FIT_MAKEBETA is called to calculate the `beta' matrix
*       -   SCULIB_FIT_MAKEALPHA is called to calculate the `alpha' matrix
*       -   The diagonal elements of the `alpha' matrix are multiplied by
*          1 + LAMBDA following the method due to Marquardt
*       -   SCULIB_INVERT_MATRIX is called to invert the modified `alpha'
*          matrix
*       -   If LAMBDA = 0 this is all that's required because this input
*          is given if the routine is required to calculate the `error'
*          matrix
*       -   Otherwise, SCULIB_FIT_MULT is called to multiply the inverted
*          `alpha' matrix by the `beta' to give the next iteration of the
*          fit for this LAMBDA
*       -   Chi-squared is calculated for the new fit and compared with
*          that for the previous fit. If the chi-squared is greater than
*          the previous value plus CHICUT then things have got worse.
*          In this case the routine will increase LAMBDA by a factor of
*          10, return the fit to the previous value and return to the
*          start of the loop. If LAMBDA exceeds 1000 status is set bad,
*          an error is reported and the loop is exited
*       -   However, if the chi-squared is an improvement then LAMBDA is
*          decreased by a factor of 10 and the loop is exited
*
*     End of loop

*  Deficiencies:

*  Bugs:


*  History:
*     23-MAR-1995: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION CHICUT

*  Arguments Given & Returned:
      DOUBLE PRECISION A (N)
      DOUBLE PRECISION LAMBDA
      DOUBLE PRECISION ALPHA (N,N)
      DOUBLE PRECISION BETA (N)
      INTEGER IK (N)
      INTEGER JK (N)
      DOUBLE PRECISION DA (N)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      EXTERNAL XISQ_ROUTINE

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION DET             ! determinant of alpha matrix
      INTEGER          I               ! DO loop index
      LOGICAL          LOOPING         ! loop control
      DOUBLE PRECISION XISQR           ! chi-squared
      DOUBLE PRECISION XISQ1           ! chi-squared

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  loop until we have achieved a smaller chi-square than input (within
*  the limits of chicut anyway)

      LOOPING = .TRUE.

      DO WHILE (LOOPING)

*  construct the beta matrix

         CALL SCULIB_FIT_MAKEBETA (XISQ_ROUTINE, N, A, BETA, STATUS)

*  and the alpha matrix

         CALL SCULIB_FIT_MAKEALPHA (XISQ_ROUTINE, N, A, ALPHA, STATUS)

*  and modify it a la Marquardt

         DO I = 1, N
            ALPHA (I,I) = (1.0D0 + LAMBDA) * ALPHA (I,I)
         END DO

*  invert the matrix

         CALL SCULIB_INVERT_MATRIX (N, ALPHA, DET, IK, JK, STATUS)

*  only calculate next iteration if lambda > 0, otherwise this routine
*  will just calculate the error matrix

         IF (LAMBDA .EQ. 0.0D0) THEN
            LOOPING = .FALSE.
         ELSE

            CALL SCULIB_FIT_MULT (N, ALPHA, BETA, DA, STATUS)
            CALL XISQ_ROUTINE (XISQ1, N, A, STATUS)

*  increment to next solution

            DO I = 1, N
               A (I) = A (I) + DA (I)
            END DO

*  calculate new chi-squared and see if it's any improvement on the old

            CALL XISQ_ROUTINE (XISQR, N, A, STATUS)
            IF (XISQR .GT. XISQ1 + CHICUT) THEN

*  oops, worse than before so return to previous solution, increase lambda
*  and try again

               DO I = 1, N
                  A (I) = A (I) - DA (I)
               END DO

               CALL XISQ_ROUTINE (XISQR, N, A, STATUS)
               LAMBDA = 10.0D0 * LAMBDA

*  cutoff in case lambda is getting too big

               IF (LAMBDA .GT. 1000.0D0) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP (' ', 'SCULIB_FIT_FUNCTION: cannot '//
     :              'reduce chi-squared even with LAMBDA above 1000',
     :              STATUS)
               END IF
            ELSE

*  an improvement so decrease lambda for the next iteration and exit the loop

               LAMBDA = 0.1D0 * LAMBDA
               LOOPING = .FALSE.
            END IF
         END IF

*  trap errors

         IF (STATUS .NE. SAI__OK) THEN
            LOOPING = .FALSE.
         END IF

      END DO

      END
