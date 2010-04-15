      SUBROUTINE IRM_NOISE( NSMP, DATA, WINDSZ, THRD, VAL, AVG, VAR,
     :                      RMS, STATUS )
*+
*  Name:
*     IRM_NOISE

*  Purpose:
*     Estimate the noise of a data series.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_NOISE( NSMP, DATA, WINDSZ, THRD, VAL, AVG, VAR,
*                     RMS, STATUS )

*  Description:
*     This subroutine is used to estimate the RMS noise of a data
*     series. To reduce the effect of the background, a window of size
*     WINDSZ is sliding along the series and the variances of the data
*     within the window at each window position are calculated. The
*     square root of the average of these local variances, excluding
*     those variances which is too far away from the average, is
*     returned as RMS noise of the data serise. The criterion to regard
*     a variance as too far away from the average is its distance from
*     the average more than THRD * (standard deviate of the variance ).

*  Arguments:
*     NSMP = INTEGER (Given)
*       The number of samples in the data series.
*     DATA( NSMP ) = REAL (Given)
*       The input data series.
*     WINDSZ = INTEGER (Given)
*       The size of the window used to calculate local variances.
*     THRD = REAL (Given)
*       The threshold used to exclude "too far away" variances from the
*       average.
*     VAL( NSMP ) = REAL (Given)
*       A temporary working array.
*     AVG( NSMP ) = REAL (Given)
*       A temporary working array.
*     VAR( NSMP ) = REAL (Given)
*       A termproary working array.
*     RMS = REAL (Given)
*       The estimated RMS noise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     15-MAR-1993 (WG):
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
      INTEGER WINDSZ
      REAL THRD

*  Arguments Returned:
      REAL VAL( NSMP )
      REAL AVG( NSMP )
      REAL VAR( NSMP )
      REAL RMS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      REAL AVGVAR                ! Average of the local variances
      REAL TEMP1, TEMP2, TEMP3, TEMP4
                                 ! temporary buffers
      INTEGER NVAL               ! Number of valid samples

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the valid samples from input data series to temporary array.
      NVAL = 0
      DO I = 1, NSMP
         IF ( DATA( I ) .NE. VAL__BADR ) THEN
            NVAL = NVAL + 1
            VAL( NVAL ) = DATA( I )
         END IF
      END DO

*  If too few valid samples in the input series, set status, report
*  and exit.
      IF ( NVAL .LT. WINDSZ ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_NOIS1', 'IRM_NOIS1: Too few valid samples'/
     :               /' in the input data series', STATUS )
         GOTO 999
      END IF

*  Calculate the local average and variance of the samples in the first
*  window.
      AVG( 1 ) = 0.0
      VAR( 1 ) = 0.0
      DO I = 1, WINDSZ
         AVG( 1 ) = AVG( 1 ) + VAL( I )
         VAR( 1 ) = VAR( 1 ) + VAL( I ) * VAL( I )
      END DO
      AVG( 1 ) = AVG( 1 ) / REAL( WINDSZ )
      VAR( 1 ) = VAR( 1 ) / REAL( WINDSZ ) - AVG( 1 ) * AVG( 1 )

*  Sliding the window along the data series and calculate the local
*  average and variance recursively.
      DO I = 2, NVAL - WINDSZ
         TEMP1 = VAL( I + WINDSZ - 1 ) - VAL( I - 1 )
         TEMP2 = VAL( I + WINDSZ - 1 ) + VAL( I - 1 )
         AVG( I ) = AVG( I - 1 ) + TEMP1 / REAL( WINDSZ )
         TEMP3 = AVG( I - 1 ) - AVG( I )
         TEMP4 = AVG( I - 1 ) + AVG( I )
         VAR( I ) = VAR( I - 1 ) + TEMP3 * TEMP4
     :                           + TEMP1 * TEMP2 / REAL( WINDSZ )
      END DO

*  Calculate the average the local variances excluding those variance
*  which deviate from the average too far away.
      CALL IRM_AVERG( NVAL - WINDSZ, VAR, 4.0, 10, AVGVAR, STATUS )

*  Take the square root of the averaged variance as the RMS noise.
      RMS = SQRT( AVGVAR )

 999  CONTINUE

      END
