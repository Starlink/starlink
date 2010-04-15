      SUBROUTINE IRM_CFF2R( NX, NY, INARR, VAR, NVAR, INVAR, BOXSZ,
     :                      NITER, CLIP, LIMITS, ILEVEL, SAMBAD,
     :                      NLIM, CINARR, OUTARR, OUTVAR, NGOOD,
     :                      SIGMA, ASUM, NSUM, STATUS )
*+
*  Name:
*     IRM_CFF2R

*  Purpose:
*     Rejects iteratively defects in a substantially smooth 2-d array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_CFF2R( NX, NY, INARR, VAR, NVAR, INVAR, BOXSZ, NITER,
*                     CLIP, LIMITS, ILEVEL, SAMBAD, NLIM, CINARR,
*                     OUTARR, OUTVAR, NGOOD, SIGMA, ASUM, NSUM, STATUS )

*  Description:
*     This routine sets pixels in a 2-d array to be invalid if they
*     deviate too far from 'local mean' and lie within a nominated
*     range of values.  The local mean is obtained by a rectangular
*     neighbourhood block-average smooth of the input array.  An
*     iterative procedure is followed where the cleaned array becomes
*     the new array to be cleaned at another threshold, and is itself
*     smoothed to find whether its pixels deviate from their smoothed
*     counterparts. A pixel that does deviate is set to the bad value,
*     as is its variance. Otherwise the pixel and variance are
*     propagated to the cleaned array.  Iterations stop and status is
*     set to SAI__ERROR when the array is entirely composed of bad
*     values or all pixels have been rejected.

*  Arguments:
*     NX = INTEGER (Given)
*        First dimension of the image to be cleaned.
*     NY = INTEGER (Given)
*        Second dimension of the image to be cleaned.
*     INARR( NX * NY ) = REAL (Given)
*        The input array containing the image from which the pixels
*        with large deviation will be rejected.
*     VAR = LOGICAL (Given)
*        The state of the variance component of the input NDF.  If the
*        variance component exists it is true.
*     NVAR = INTEGER (Given)
*        The number of elements of the variance array.
*     INVAR( NVAR ) = REAL (Given)
*        The variance array of the input array to be cleaned.  If the
*        variance is present the dimensions should be NX by NY, as for
*        the data arrays.
*     BOXSZ( 2 ) = INTEGER (Given)
*        The size of smoothing box in pixels.
*     NITER = INTEGER (Given)
*        The number of iterations in the rejection algorithm.
*     CLIP( NITER ) = REAL (Given)
*        The maximum number of standard deviations for the rejection
*        threshold at each iteration.
*     LIMITS( 2 ) = REAL (Given)
*        The thresholds outside which any pixel data value will not be
*        cleaned. The order is not important.
*     ILEVEL = INTEGER (Given)
*        The interaction level.  If it is greater than one, the
*        intermediate result of each iteration will be reported to the
*        user.
*     SAMBAD = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then bad input
*        pixels will be propagated to the output image unchanged during
*        smoothing (a smoothed output value will be calculated for all
*        other pixels). If a .FALSE. value is given, then the NLIM
*        argument determines whether an output pixel is good or bad.
*        The value of NLIM is not relevant if SAMBAD is .TRUE.
*     NLIM = INTEGER (Given)
*        Minimum number of good pixels which must be present in the
*        smoothing box in order to calculate a smoothed output pixel.
*        If this minimum number is not satisfied, then a bad output
*        pixel will result. A value between 1 and the total number of
*        pixels in the smoothing box should be supplied.
*     CINARR( NX * NY ) = REAL (Given)
*        Work array for containing the latest cleaned iteration,
*        initially the input array to be supplied to the local-mean
*        routine.  Thus on exit it contains the cleaned image before the
*        last iteration.
*     OUTARR( NX * NY ) = REAL (Returned)
*        The output array to contain the processed image.
*     OUTVAR( NVAR ) = REAL (Returned)
*        The variance of the output array.
*     NGOOD = INTEGER (Returned)
*        The valid pixels in the output array. If input array has no
*        valid pixel at all, it will be set to -1.
*     SIGMA = DOUBLE PRECISION (Returned)
*        The estimate of the RMS noise per pixel in the output image.
*     ASUM( NX ) = REAL (Returned)
*        Work array for pixel sums during smoothing the image.
*     NSUM( NX ) = INTEGER (Returned)
*        Work array for counting good pixels during smoothing the image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R. F. Warren-Smith (STARLINK)
*     WG: Wei Gong (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1992 (DSB):
*        Original version, copied from KAPPA routine KPS1_CFF2x, written
*        by RFWS, WG and MJC.
*     23-JUN-1994 (DCP):
*        Modified as per MJC's mod to KPS1_CFF2R of 17-DEC-1992 ie
*        Fixed bug that caused variance array to be accessed when it was
*        not present.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing.

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'MSG_PAR'          ! MSG constants.

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL    INARR( NX * NY )
      LOGICAL VAR
      INTEGER NVAR
      REAL    INVAR( NVAR )
      INTEGER BOXSZ( 2 )
      INTEGER NITER
      REAL    CLIP( NITER )
      REAL    LIMITS( 2 )
      INTEGER ILEVEL
      LOGICAL SAMBAD
      INTEGER NLIM
      REAL    CINARR( NX * NY )

*  Arguments Returned:
      REAL    OUTARR( NX * NY )
      REAL    OUTVAR( NVAR )
      INTEGER NGOOD
      DOUBLE PRECISION SIGMA
      REAL    ASUM( NX )
      INTEGER NSUM( NX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Max. dimension of image the routine
                                 ! can handle.
      PARAMETER ( NDIM = 2 )
      REAL Q0                    ! Minimum variance of a pixel can take.
      PARAMETER ( Q0 = 0.0 )     ! Set as the quantisation error
                                 ! variance.

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER IBOX( NDIM )       ! Half size of the smoothing box
      INTEGER OBOX( NDIM )       ! Odd size of the smoothing box
      INTEGER ITER               ! Iteration loop index
      INTEGER NBOX               ! Number of pixels in the box
      INTEGER NEL                ! Number of pixels in the array
      INTEGER NREJ               ! Number of rejected pixels in total
      INTEGER NSTART             ! Number of valid pixels in input array
      INTEGER NEXCL              ! Number of excluded (outside value
                                 ! limits) pixels in total

      REAL DIFF                  ! Image difference
      REAL DIFF2                 ! Square of image difference
      REAL CLIPSQ                ! Squared clipping threshold
      REAL RANGE( 2 )            ! Ordered limits between which a
                                 ! pixel's data value must lie if has a
                                 ! chance of being rejected
      REAL SIG                   ! Sum of squared difference of images
      REAL THRESH                ! Threshold for rejecting algorithm
      REAL VARNCE                ! Estimated Variance of the image

      LOGICAL BAD                ! Bad-pixel flag of the smoothed image

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise variables
      NEXCL = 0
      THRESH = 0.0
      VARNCE = 0.0

*  Make sure the box size is odd, and count the number of pixels in the
*  box.
      NBOX=1
      DO 5 I =1, NDIM
         IBOX( I ) = MAX( 0, BOXSZ( I ) / 2 )
         OBOX( I ) = 2 * IBOX( I ) + 1
         NBOX = NBOX * OBOX( I )
    5 CONTINUE

*  Ensure the range has correct polarity.
      RANGE( 1 ) = MIN( LIMITS( 1 ), LIMITS( 2 ) )
      RANGE( 2 ) = MAX( LIMITS( 1 ), LIMITS( 2 ) )

*  Copy input array to work array, couting the number of valid pixels.
      NSTART = 0
      NEL = NX * NY
      DO 10 I = 1, NEL
         IF ( INARR( I ) .NE. VAL__BADR ) THEN
            NSTART = NSTART + 1
         END IF
         CINARR( I ) = INARR( I )
   10 CONTINUE

*  If interaction level is high, report the number of valid pixels.
      IF ( ILEVEL .GE. 2 ) THEN
         CALL MSG_SETI( 'NSTART', NSTART )
         CALL MSG_OUTIF( MSG__NORM, 'IRM_CFF2R_MSG1',
     :     '  Input image initially has ^NSTART valid pixels.', STATUS )
      END IF

*  If there is no valid pixel, set NGOOD to -1 and exit.
      IF ( NSTART .EQ. 0 ) THEN
         NGOOD = -1
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_CFF2R_ERR1',
     :       'IRM_CFF2R: There are no valid pixels in the input image.',
     :                  STATUS )
         GOTO 999
      END IF

*  Perform the rejection iteration.
      DO 20 ITER = 1, NITER

*  Smooth the 2-d dimensional image using a block average of the
*  rectangular neighbourhood.
         CALL IRM_BLOCR( .TRUE., SAMBAD, .FALSE., NX, NY, CINARR,
     :                   IBOX( 1 ), IBOX( 2 ), NLIM, OUTARR, BAD,
     :                   ASUM, NSUM, STATUS )

*  On the first iteration, compare the smoothed image with the original
*  and derive a noise estimation.  This is not required if there is a
*  known variance.
         IF ( .NOT. VAR .AND. ITER .EQ. 1 ) THEN
            SIG = 0.0
            NGOOD = 0
            DO 30 I = 1, NEL

*  Use only those pixels valid in both image.
               IF ( INARR( I ) .NE. VAL__BADR .AND.
     :              OUTARR( I ) .NE. VAL__BADR ) THEN
                  IF ( INARR( I ) .GE. RANGE( 1 ) .AND.
     :                 INARR( I ) .LE. RANGE( 2 ) ) THEN
                     DIFF = INARR( I ) - OUTARR( I )
                     SIG = SIG + DIFF * DIFF

*  Count the number of the valid pixels in both arrays.
                     NGOOD = NGOOD + 1
                  END IF
               END IF
   30       CONTINUE

*  Estimate the variance of the image.
            IF ( NGOOD .GE. 1 ) THEN
               VARNCE = MAX( SIG / NUM_ITOR( NGOOD ), Q0 )
            ELSE
               VARNCE = Q0
            END IF
         END IF

*  For efficiency compute the square clipping threshold.
         CLIPSQ = NUM_RTOR( CLIP( ITER ) * CLIP( ITER ) )

*  Perform the the cleaning.
*  =========================

*  Initialise statistics.
         SIG = 0.0
         NGOOD = 0
         NEXCL = 0

*  Loop for each array element.
         DO 40 I = 1, NEL

*  If the output array is already bad there is no more work to be done
*  to the current pixel.
            IF ( OUTARR( I ) .NE. VAL__BADR ) THEN

*  The input pixel must be good and within the value limits for cleaning
*  to occur.
               IF ( CINARR( I ) .NE. VAL__BADR ) THEN

                  IF ( CINARR( I ) .GE. RANGE( 1 ) .AND.
     :                 CINARR( I ) .LE. RANGE( 2 ) ) THEN

*  If the variance component exists and the value corresponding to the
*  pixel is valid, set threshold according to it.
                     IF ( VAR ) THEN
                        IF ( INVAR( I ) .NE. VAL__BADR ) THEN
                           THRESH = CLIPSQ * INVAR( I )
                        END IF
                     ELSE

*  Otherwise set the threshold according to the estimated variance.
                        THRESH = CLIPSQ * VARNCE
                     END IF

*  Form the statistics.
                     DIFF = INARR( I ) - OUTARR( I )
                     DIFF2 = DIFF * DIFF

*  Does the pixel lie witihn the threshold?
                     IF ( DIFF2 .LE. THRESH ) THEN

*  It does so copy those pixels to output array whose deviation from
*  its local-mean within the threshold, sum the square error of the
*  output array and count the number of valid pixels in the output
*  array.
                        OUTARR( I ) = INARR( I )
                        SIG = SIG + DIFF2
                        NGOOD = NGOOD + 1
                     ELSE

*  Set those pixels invalid which deviating from its local-mean by more
*  that the threshold.
                        OUTARR( I ) = VAL__BADR
                     END IF
                  ELSE

*  Merely copy those pixels to output array who lie outside the range
*  of values to be cleaned.  Keep a count of these so they are not
*  confused with rejected or bad pixels.
                     OUTARR( I ) = INARR( I )
                     NEXCL = NEXCL + 1
                  END IF
               ELSE

*  Propagate invalid pixels of the input array of this iteration to
*  result array.
                  OUTARR( I ) = VAL__BADR
               END IF
            END IF
  40     CONTINUE

*  Evaluate the variance estimation of the result array.
         IF ( NGOOD .GE. 1 ) THEN
            VARNCE = MAX( SIG / NUM_ITOR( NGOOD ), Q0 )
         ELSE
            VARNCE = Q0
         END IF

*  Estimate the RMS noise per pixel of the result array.
         SIGMA = SQRT( NUM_RTOD( VARNCE ) * DBLE( NBOX ) /
     :           DBLE( MAX( 1, NBOX-1 ) ) )

*  If interaction level is high, report the progress of the iteration.
         IF ( ILEVEL .GE. 2 ) THEN
            CALL MSG_SETR( 'SIGMA', NUM_DTOR( SIGMA ) )
            CALL MSG_SETI( 'VALID', NGOOD + NEXCL )
            CALL MSG_SETI( 'ITER', ITER )
            CALL MSG_OUTIF( MSG__NORM, 'IRM_CFF2R_MSG2',
     :        '  Iteration ^ITER has ^VALID valid pixels left and '/
     :        /'SIGMA = ^SIGMA.', STATUS )
         END IF

*  If all pixels has been rejected, exit.
         IF ( NGOOD .LE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRM_CFF2R_ERR2',
     :        'IRM_CFF2R: All pixels have been rejected.', STATUS )
            GOTO 999
         END IF

*  If this is not the last iteration, then copy the result of last
*  iteration to input work array to begin the next iteration.
         IF ( ITER .NE. NITER ) THEN
            DO 45 I = 1, NEL
               CINARR( I ) = OUTARR( I )
   45       CONTINUE
         END IF
   20 CONTINUE

*  If interaction level is high, report to user how many pixels were
*  rejected after all iterations are complete.
      IF ( ILEVEL .GE. 2 ) THEN
         NREJ = NSTART - NGOOD - NEXCL
         CALL MSG_SETI( 'NREJ', NREJ )
         CALL MSG_OUTIF( MSG__NORM, 'IRM_CFF2R_MSG3',
     :     '  ^NREJ pixels were rejected in total.', STATUS )
      END IF

*  Propagate the variance from the input image to the output, if the
*  variance component exists.
      IF ( VAR ) THEN
         DO 50 I = 1, NEL
            IF ( OUTARR( I )  .NE.  VAL__BADR .AND.
     :           INVAR( I ) .NE.  VAL__BADR ) THEN
               OUTVAR( I ) = INVAR( I )
            ELSE

*  When pixel of a output array is invalid or the variance of that
*  pixel of the input array is invalid, set the variance of the pixel
*  of the output array invalid.
               OUTVAR( I ) = VAL__BADR
            END IF
   50    CONTINUE
      END IF

  999 CONTINUE

*  End the routine.

      END
