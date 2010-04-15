      SUBROUTINE JCMT_CONVOLVE_ERRORS (A, A_ERR, B, N1, N2, NMID,
     :   ABAD, RNORM, R, R_ERR, STATUS)
*+
*  Name:
*     JCMT_CONVOLVE_ERRORS

*  Purpose:
*     Convolve array A by Array B to give result in R. Input errors in
*     A_ERR are propagated into R_ERR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_CONVOLVE (A, A_ERR, B, N1, N2, NMID, ABAD, RNORM, R,
*    :   R_ERR, STATUS)

*  Description:
*     Makes a convolution of Array A by array B using simple
*     multiplication (i.e. no FFTs are used) The routine assumes that
*     array A is of length N1 and array B is of length N2 and that the
*     centre of array B is at NMID. The final result is normalised by
*     dividing by RNORM. Elements of A that are flagged bad by being
*     set equal to ABAD are ignored, the same as if they were 0 on
*     input. The input errors are propagated as:-
*
*     r_err = sqrt (sum[convolution_fn * a_err)**2] / rnorm
*
*     Errors associated with bad data points are ignored. After this
*     process the errors can no longer be considered independent.
*
*     This routine is essentially a rewritten version of XCONV from the
*     RESTOR program in the NOD2 package by Haslam (1974) Astron.
*     Astrophys. Suppl. 15 p333

*  Arguments:
*     A( N1 ) = REAL (Given)
*        Input data array
*     A_ERR( N1 ) = REAL (Given)
*        Input error array
*     B( N2 ) = REAL (Given)
*        Convolution function
*     N1 = INTEGER (Given)
*        length of input function
*     N2 = INTEGER (Given)
*        Length of convolution function
*     NMID = INTEGER (Given)
*        index of zero point of convolution function
*     ABAD = REAL (Given)
*        flag value that signals elements to be ignored in A
*     RNORM = REAL (Given)
*        normalization factor for convolution function
*     R( N1 ) = REAL (Returned)
*        result of the convolution
*     R_ERR( N1 ) = REAL (Returned)
*        convolved errors

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL
*     {enter_new_authors_here}

*  History:
*     27-AUG-1991:
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER N1, N2, NMID
      REAL A (N1), A_ERR (N1), B (N2), ABAD, RNORM

*  Arguments Returned:
      REAL R (N1), R_ERR (N1)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER JK                 ! counter
      REAL SUM                   ! convolution sum
      REAL ERSUM                 ! error convolution sum
      INTEGER KL                 ! counter
      INTEGER JK1                ! pointer into convolution array

*.

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

*  A little whiff of NOD2 code

      DO JK = 1, N1
         SUM = 0
         ERSUM = 0
         DO KL = MIN(1,1+NMID-JK), N2
            JK1 = JK - NMID + KL
            IF (JK1.LE.N1 .AND. JK1.GE.1) THEN
               IF (A(JK1) .NE. ABAD) THEN
                  SUM = SUM + A(JK1)*B(KL)
                  ERSUM = ERSUM + (A_ERR(JK1)*B(KL))**2
               END IF
            END IF
         END DO
         R(JK) = SUM / RNORM
         IF (ERSUM .GT. 0.0) THEN
            R_ERR(JK) = SQRT(ERSUM) / RNORM
         ELSE
            R_ERR(JK) = 0.0
         END IF
      END DO

      END


