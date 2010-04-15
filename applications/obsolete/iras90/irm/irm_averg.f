      SUBROUTINE IRM_AVERG( NSMP, DATA, THRD, NITRN, AVG, STATUS )
*+
*  Name:
*     IRM_AVERG

*  Purpose:
*     Estimate the average of a data series.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_AVERG(  NSMP, DATA, THRD, NITRN, AVG, STATUS )

*  Description:
*     This subroutine estimates the average of a data series by
*     iteratively calculating the average of the data series with the
*     data samples which are too far away from the current data average
*     being excluded. The samples whose distances from the current data
*     average are more than THRD * (Current standard deviation ) will
*     be regarded as too far from the current average and will be
*     excluded from the next iteration to calculate the new average and
*     standard deviation. This procedure will be repeated until the
*     successive standard deviations are the same or specified iteration
*     number (NITRN) is rearched.

*  Arguments:
*     NSMP = INTEGER (Given)
*        Number of samples in the input data series.
*     DATA( NSMP ) = REAL (Given)
*        The inut data series.
*     THRD = REAL (Given)
*        The multiple of the standard deviation. Samples will not be
*        included in the calculation of the average and standart
*        deviation of the next iteration if they are more than this
*        multiple away from the current average.
*     NITRN = INTEGER (Given)
*        Max. number of iterations at which the standard deviation will
*        be returned as the estimated noise no matter the convergence
*        has achieved.
*     AVG = REAL (Returned)
*        The average of the data series.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      INTEGER NSMP
      REAL DATA( NSMP )
      REAL THRD
      INTEGER NITRN

*  Arguments Returned:
      REAL AVG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      INTEGER ITERAT             ! Number of iteration has been done
      INTEGER NSUM               ! Number of samples summed
      REAL PSGM                  ! Previous standard deviation
      REAL SGM                   ! Standard deviate
      REAL SUM                   ! Sum of samples
      REAL SQSUM                 ! Sum of squared samples
      REAL THRSHD                ! Threshold for excluding samples

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the current threshood so that all samples are included in
*  the calculation of the first iteration. Initialise the current and
*  prvious standard deviation so that they are not the same before
*  and after the first iteration to prevent subroutine from exit. And
*  initialise the number of iterations.
      THRSHD = VAL__MAXR
      AVG = 0.0
      SGM = -1.0
      PSGM = 1.0
      ITERAT = 0

*  Iteratively calculate the standard deviation until the max. iteration
*  number have been rearched or the successive standard deviations are
*  the same.
      DO WHILE ( ITERAT .LT. NITRN .AND.
     :           ABS( SGM - PSGM ) .GT. VAL__SMLR )
         ITERAT = ITERAT + 1

*  Record the standard deviation obtained in the previous iteration.
         PSGM = SGM

*  Calculate the sum of the data sample and the sum of the squared
*  sample.
         NSUM = 0
         SUM = 0.0
         SQSUM = 0.0
         DO I = 1, NSMP

*  Exclude bad samples and the samples which is move than the threshold
*  away from the average.
            IF ( DATA( I ) .NE. VAL__BADR .AND.
     :           ABS( DATA( I ) - AVG ) .LE. THRSHD ) THEN
               SUM = SUM + DATA( I )
               SQSUM = SQSUM + DATA( I ) * DATA( I )
               NSUM = NSUM + 1
            END IF
         END DO

*  If all data samples are rejected, the data series might only contains
*  bad samples or the threshold is not correctly selected, set status,
*  report and exit.
         IF ( NSUM .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRM_AVERG_ERR1', 'IRM_NOISE: Data series '/
     :                   /'contains no valid data or the threshold '/
     :                   /'for excluding data is not properly '/
     :                   /'selected (possibly programming error).',
     :                     STATUS )
            GOTO 999
         END IF

*  Get the current average and current standard deviation.
         AVG = SUM / REAL( NSUM )
         SGM = SQRT( SQSUM / REAL( NSUM ) - AVG * AVG )

*  Update the threshold.
         THRSHD = THRD * SGM
      END DO

 999  CONTINUE

      END
