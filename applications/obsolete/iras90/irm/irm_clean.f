      SUBROUTINE IRM_CLEAN( LBND, UBND, DATIN, LBNDW, UBNDW, HBOX,
     :                      NITER, NSIGMA, DATOUT, RMS, WORK1, WORK2,
     :                      WORK3, STATUS )
*+
*  Name:
*     IRM_CLEAN

*  Purpose:
*     Clean a one dimensional data array by removing bright sources.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_CLEAN( LBND, UBND, DATIN, LBNDW, UBNDW, HBOX, NITER,
*                     NSIGMA, DATOUT, RMS, WORK1, WORK2, WORK3, STATUS )

*  Description:
*     The output data is a copy of the input data except that bright
*     sources are replaced by bad values.
*
*     The input data is smoothed with a box filter of given width, and
*     the residuals between the smoothed data and the original data are
*     found. The RMS residual is then found and data values are set bad
*     if they differ from the smoothed data values by more than a
*     specified multiple of the RMS residual. The process is then
*     repeated NITER times. An error is reported if all the data is bad
*     on entry  or if all the data is removed by the cleaning algorithm.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the data.
*     UBND = INTEGER (Given)
*        The upper bound of the data.
*     DATIN( LBND : UBND ) = REAL (Given)
*        The input data.
*     LBNDW = INTEGER (Given)
*        The lower bound of the work arrays. This should be equal to
*        LBND - HBOX - 1.
*     UBNDW = INTEGER (Given)
*        The upper bound of the work arrays. This should be equal to
*        UBND + HBOX.
*     HBOX = INTEGER (Given)
*        Half the width of the smoothing box. The full size is
*        2*HBOX+1.
*     NITER = INTEGER (Given)
*        The number of cleaning iterations to perform.
*     NSIGMA = REAL (Given)
*        The multiple of the RMS residual at which points will be
*        rejected.
*     DATOUT( LBND : UBND ) = REAL (Returned)
*        The cleaned data.
*     RMS = REAL (Returned)
*        An estimate of the standard deviation of the noise in the input
*        data.
*     WORK1( LBNDW : UBNDW ) = REAL (Returned)
*        Work space.
*     WORK2( LBNDW : UBNDW ) = REAL (Returned)
*        Work space.
*     WORK3( LBNDW : UBNDW ) = INTEGER (Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      REAL DATIN( LBND : UBND )
      INTEGER LBNDW
      INTEGER UBNDW
      INTEGER HBOX
      INTEGER NITER
      REAL NSIGMA

*  Arguments Returned:
      REAL DATOUT( LBND : UBND )
      REAL RMS
      REAL WORK1( LBNDW : UBNDW )
      REAL WORK2( LBNDW : UBNDW )
      INTEGER WORK3( LBNDW : UBNDW )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL BOXVAL                ! Sum of ALL data values in the box.
      REAL BOXWGT                ! No. of VALID values in the box.
      REAL CLIP                  ! Residual threshold at which to clip.
      INTEGER I                  ! Pixel index
      REAL INVAL                 ! Input data value.
      INTEGER ITER               ! Current iteration.
      INTEGER NSUM               ! No. of values summed in SUMSQ.
      REAL RESID                 ! Residual between smoothed and
                                 ! un-smoothed data.
      REAL SUMSQ                 ! Sum of squared residuals.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the half box size is zero, or if zero iterations are to be
*  performed, just copy the input to the output.
      IF( NITER .EQ. 0 .OR. HBOX .EQ. 0 ) THEN

         DO I = LBND, UBND
            DATOUT( I ) = DATIN( I )
         END DO

         GO TO 999

      END IF

*  Copy the input data to array WORK1, setting invalid pixels to the
*  value zero. Create an array WORK3 in which each pixel has value 1
*  if the corresponding pixel in WORK1 is valid, and 0 if it is not.
      DO I = LBND, UBND
         INVAL = DATIN( I )

         IF( INVAL .NE. VAL__BADR ) THEN
            WORK1( I ) = INVAL
            WORK3( I ) = 1

         ELSE
            WORK1( I ) = 0.0
            WORK3( I ) = 0

         END IF

      END DO

*  Pad the supplied data with a half box width of invalid data at each
*  end so that the convolution has data to work on.
      DO I = LBND - HBOX - 1, LBND - 1
         WORK1( I ) = 0.0
         WORK3( I ) = 0
      END DO

      DO I = UBND + 1, UBND + HBOX
         WORK1( I ) = 0.0
         WORK3( I ) = 0
      END DO

*  Loop round doing the required number of rejection iterations...
      DO ITER = 1, NITER

*  Find the sum of the data values covered by the half box width at
*  the start of the array.
         BOXVAL = 0.0
         BOXWGT = 0

         DO I = LBND, HBOX
            BOXVAL = BOXVAL + WORK1( I )
            BOXWGT = BOXWGT + WORK3( I )
         END DO

*  Initialise the sums used later to form the RMS residual.
         SUMSQ = 0.0
         NSUM = 0

*  Now loop round the array.
         DO I = LBND, UBND

*  The leading pixel value is added into the box value and the
*  trailing pixel value is removed.
            BOXVAL = BOXVAL + WORK1( I + HBOX ) - WORK1( I - HBOX - 1 )

*  The same is done with the weights so that BOXWGT is equal to
*  the number of good values contributing to BOXVAL.
            BOXWGT = BOXWGT + WORK3( I + HBOX ) - WORK3( I - HBOX - 1 )

*  If BOXVAL contains any good values, and if the central data value in
*  the box is valid, normalise BOXVAL to get the mean data value in the
*  box, and form the residual between the mean value and the central
*  value. This residual is stored for later use.  Increment the sums
*  used later to form the RMS residual.
            IF( BOXWGT .GT. 0 .AND. WORK3( I ) .GT. 0 ) THEN
               RESID = WORK1( I ) - ( BOXVAL/REAL( BOXWGT ) )
               WORK2( I ) = RESID
               SUMSQ = SUMSQ + RESID**2
               NSUM = NSUM + 1
            END IF

         END DO

*  Form the RMS residual between the smoothed data and the un-smoothed
*  data.
         IF( NSUM .GT. 0.0 ) THEN
            RMS = SQRT( SUMSQ/NSUM )
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', ITER )
            CALL ERR_REP( 'IRM_CLEAN_ERR1',
     :              'IRM_CLEAN: No good data found during iteration ^N',
     :                    STATUS )
            GO TO 999
         END IF

*  Go through the un-smoothed data assigning a value and weight of zero
*  to any values which differ by more than the required number of
*  standard deviations from the corresponding smoothed value.
         CLIP = NSIGMA * RMS
         DO I =LBND, UBND
            IF( ABS( WORK2( I )  ) .GT. CLIP ) THEN
               WORK1( I ) = 0.0
               WORK3( I ) = 0
            END IF
         END DO

*  Do the next iteration.
      END DO

*  Copy the remaining data to the output
      DO I = LBND, UBND
         IF( WORK3( I ) .GT. 0 ) THEN
            DATOUT( I ) = DATIN( I )
         ELSE
            DATOUT( I ) = VAL__BADR
         END IF
      END DO

*  If an error has occurred, give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_CLEAN_ERR2',
     :   'IRM_CLEAN: Unable to remove sources from a 1-D array of data',
     :                 STATUS )
      END IF

      END
