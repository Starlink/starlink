      SUBROUTINE ECH_DECOS_PREDICT(
     :           NX,
     :           NY,
     :           IMAGE,
     :           QUALITY,
     :           ENERGY,
     :           ENERGY_PER_INC,
     :           COUNT_PER_X,
     :           PERCENTAGE,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           PROCESS,
     :           IGNORED,
     :           COUNT,
     :           EXPECTED_PERCENT,
     :           PIXEL_COUNT,
     :           MEAN,
     :           SIGMA,
     :           MCOUNT,
     :           MEAN_ENERGY,
     :           Y_TRACE_COORD,
     :           DATA,
     :           INDEX_X,
     :           INDEX_Y,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_DECOS_PREDICT

*  Purpose:
*     Calculated predicted probability of observed CDF.

*  Description:
*     This routine calculates the predicted probability that the observed
*     deviation from the expected gaussian CDF is as observed.

*  Invocation:
*     CALL ECH_DECOS_PREDICT(
*     :    NX,
*     :    NY,
*     :    IMAGE,
*     :    QUALITY,
*     :    ENERGY,
*     :    ENERGY_PER_INC,
*     :    COUNT_PER_X,
*     :    PERCENTAGE,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    PROCESS,
*     :    IGNORED,
*     :    COUNT,
*     :    EXPECTED_PERCENT,
*     :    PIXEL_COUNT,
*     :    MEAN,
*     :    SIGMA,
*     :    MCOUNT,
*     :    MEAN_ENERGY,
*     :    Y_TRACE_COORD,
*     :    DATA,
*     :    INDEX_X,
*     :    INDEX_Y,
*     :    STATUS
*     :    )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     DEK_BELOW = INTEGER (Given)
*        Dekker distance below order traces.
*     DEK_ABOVE = INTEGER (Given)
*        Dekker distance above order traces.
*     DATA = REAL (Temporary Workspace)
*        Ratios of observed/predicted energy.
*     INDEX_X = SHORT (Temporary Workspace)
*        X coords of pixels.
*     INDEX_Y = SHORT (Temporary Workspace)
*        Y coords of pixels.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coordinates of trace.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.
*     QUALITY = LOGICAL (Given)
*        Input data frame quality flags array.
*     ENERGY = REAL (Given and Returned)
*        Total counts in an increment.
*     ENERGY_PER_INC = REAL (Given and Returned)
*        Total flux in an increment.
*     COUNT_PER_X = INTEGER (Given and Returned)
*        Pixels used per increment.
*     PERCENTAGE = REAL (Given and Returned)
*        Percentage of energy present.
*     PROCESS = LOGICAL (Given and Returned)
*        Set TRUE when increment is being processed.
*     IGNORED = LOGICAL (Given and Returned)
*        Set TRUE if spatial location to be ignored.
*     COUNT = INTEGER (Given)
*        Number of bytes to copy.
*     EXPECTED_PERCENT = REAL (Given and Returned)
*        Expected flux as a percentage of total in increment.
*     PIXEL_COUNT = INTEGER (Given and Returned)
*        Number of contributing pixels.
*     MEAN = REAL (Given and Returned)
*        Mean flux.
*     SIGMA = REAL (Given and Returned)
*        Deviation on mean flux.
*     MCOUNT = INTEGER (Given and Returned)
*        Number of pixels contributing to mean.
*     MEAN_ENERGY = REAL (Given and Returned)
*        Mean flux.

*  Method:
*     Clear working arrays, counters
*     Loop through increments from below to above order trace
*        Determine 'energy' in this increment (if OK to process)
*     End loop
*     Calculate mean energy percentage per increment
*     If only one pixel increment is available, set all percentages to 50%
*     Else
*        Loop through increments from below to above order trace
*           If processing increment then
*              Loop through all pixels in this increment (ie at all x)
*                 If pixel coords OK, BUT pixel has already been clipped then
*                    Calculate gross estimate of percentage expected in pixel
*                 Endif
*              End loop
*           Endif
*        End loop
*     Endif
*     Loop through increments from below to above order trace
*        If processing increment this time, and pixel coords in range then
*              If image pixel OK then
*                 Add contribution to expected percentage for increment
*              Endif
*        Endif
*        Normalise by number of pixels contributing this increment
*     End loop
*     Renormalise expected percentages to balance for any missing increments
*     Initialise full order pixel counters
*     Loop through increments from below to above order trace
*        If processing increment this time, and pixel coords in range then
*              If image pixel OK then
*                Add pixel and its ratio of expected/observed intensity to data set
*                Update totals for statistics
*                Keep record of pixels location on image
*              Endif
*        Endif
*     End loop
*     Calculate mean and sigma of observed data set

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     16-JUL-1996 (MJC):
*       New prologue.  Handling for possible BAD values in IMAGE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_QUALITIES.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      BYTE QUALITY( NX, NY )
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE

*  Arguments Returned:
      INTEGER MCOUNT

*  Workspace:
      REAL DATA( NX * MAX_SLICE_PIXELS )
*          ! Ratios of observed/predicted energy.
      INTEGER*2 INDEX_X( NX * MAX_SLICE_PIXELS )
*          ! X coords of pixels.
      INTEGER*2 INDEX_Y( NX * MAX_SLICE_PIXELS )
*          ! Y coords of pixels.
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
       INTEGER STATUS

*  Local Variables:
      REAL EXPECTED_PERCENT( -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! Predicted percentage per order increment.
      REAL ENERGY_PER_INC(  -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! Predicted energy per order increment.
      REAL ENERGY( NX )
      REAL PERCENTAGE( NX )
      REAL MEAN
      REAL SIGMA
      REAL MEAN_ENERGY
      REAL EXPCTOT
      REAL TOTAL
      REAL TOTSQ

      INTEGER COUNT( -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! Count of good pixels per order increment.
      INTEGER COUNT_PER_X( NX )
      INTEGER I
      INTEGER IY
      INTEGER IX
      INTEGER IY_DELTA
      INTEGER IQUALITY
      INTEGER PIXEL_COUNT

      LOGICAL IGNORED( -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! TRUE if order increment has no good pixels.
      LOGICAL PROCESS( -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! TRUE if increment is being processed.

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Clear working arrays, counters.
      DO IX = 1, NX
         ENERGY( IX ) = 0.0
         PERCENTAGE( IX ) = 0.0
         COUNT_PER_X( IX ) = 0.0
      END DO
      MEAN_ENERGY = 0.0
      MCOUNT = 0

*  Loop through increments from below to above order trace.
      DO IY_DELTA = DEK_BELOW, DEK_ABOVE

*     Determine 'energy' in this increment (if OK to process).
         ENERGY_PER_INC( IY_DELTA ) = 0.0
         IF ( PROCESS( IY_DELTA ) .AND.
     :        ( .NOT. IGNORED( IY_DELTA ) ) ) THEN
            DO I = 1, NX
               IY = INT( Y_TRACE_COORD( I ) + 0.5 ) + IY_DELTA
               IF ( IY .GT. 0 .AND. IY .LE. NY ) THEN
                  IF ( QUALITY ( I, IY ) .EQ. 0 .AND.
     :                 IMAGE( I, IY ) .NE. ECH__BAD_REAL ) THEN

*                 'energy' is just the sum of counts in all 'good' pixels
*                 summed along constant x=??.
                     ENERGY( I ) = ENERGY( I ) +
     :                     MAX( 0., IMAGE( I, IY ) )

*                 'energy_per_inc' is the sum of counts in all 'good'
*                 pixels, summed along constant delta-y from the central
*                 order trace.
                     ENERGY_PER_INC( IY_DELTA ) =
     :                     ENERGY_PER_INC( IY_DELTA ) +
     :                     MAX( 0., IMAGE( I, IY ) )

*                 'count_per_x' is used to normalise 'energy_per_inc'.
                     COUNT_PER_X( I ) = COUNT_PER_X( I ) + 1

*                 'percentage' is sum percentage of expectation values
*                 at each x=?? step.
                     PERCENTAGE( I ) = PERCENTAGE( I ) +
     :                     EXPECTED_PERCENT( IY_DELTA )

*                 'mean_energy' is mean pixel value over all good
*                 pixels considered this order.
                     MEAN_ENERGY = MEAN_ENERGY +
     :                     MAX( 0., IMAGE( I, IY ) )
                     MCOUNT = MCOUNT + 1
                  END IF
               END IF
            END DO
         END IF
      END DO

*  Calculate mean energy percentage per increment.
      MEAN_ENERGY = MEAN_ENERGY / FLOAT( MAX( 1, MCOUNT ) )

*  If only one pixel increment is available, set all percentages to 50%
      IF ( DEK_ABOVE - DEK_BELOW .LE. 2 ) THEN
         DO I = 1, NX
            ENERGY( I ) = 2.0 * MEAN_ENERGY
            PERCENTAGE( I ) = 0.5
         END DO

      ELSE

*     Loop through increments from below to above order trace.
         DO IY_DELTA = DEK_BELOW, DEK_ABOVE

*        If processing increment then.
            IF ( PROCESS( IY_DELTA ) .AND.
     :           .NOT. IGNORED( IY_DELTA ) ) THEN

*           Loop through all pixels in this increment (ie at all x).
               DO I = 1, NX
                  IY = INT( Y_TRACE_COORD( I ) + 0.5 ) + IY_DELTA

*              If pixel coords OK, BUT pixel has already been clipped then.
                  IF ( IY .GT. 0 .AND. IY .LE. NY ) THEN
                     IQUALITY = QUALITY( I, IY )
                     IF ( IAND( IQUALITY, QTY_COSMIC_RAY ) .EQ.
     :                    QTY_COSMIC_RAY .AND.
     :                    PERCENTAGE( I ) .GT. 0.0 ) THEN

*                    Calculate gross estimate of percentage expected in pixel.
                        ENERGY_PER_INC( IY_DELTA ) =
     :                        ENERGY_PER_INC( IY_DELTA ) +
     :                        ENERGY( I ) / PERCENTAGE( I ) *
     :                        EXPECTED_PERCENT( IY_DELTA )
                        ENERGY( I ) = ENERGY( I ) +
     :                        ENERGY( I ) / PERCENTAGE( I ) *
     :                        EXPECTED_PERCENT( IY_DELTA )
                        PERCENTAGE( I ) = PERCENTAGE( I ) +
     :                        EXPECTED_PERCENT( IY_DELTA )
                     END IF
                  END IF
               END DO
            END IF
         END DO
      END IF

*  Loop through increments from below to above order trace.
      DO IY_DELTA = DEK_BELOW, DEK_ABOVE
         COUNT ( IY_DELTA ) = 0
         EXPECTED_PERCENT( IY_DELTA ) = 0.0

*     If processing increment this time, and pixel coords in range then.
         IF ( process( iy_delta ) ) THEN
            DO i = 1, nx
               IY = INT( Y_TRACE_COORD( I ) + 0.5 ) + IY_DELTA
               IF ( IY .GT. 0 .AND. IY .LE. NY ) THEN

*           If image pixel OK then.
                  IF ( QUALITY( I, IY ) .EQ. 0 .AND.
     :                 IMAGE( I, IY ) .NE. ECH__BAD_REAL .AND.
     :                 ENERGY ( I ) .NE. 0.0 ) THEN

*              Add contribution to expected percentage for increment.
                     EXPECTED_PERCENT( IY_DELTA ) =
     :                     EXPECTED_PERCENT( IY_DELTA ) +
     :                     MAX( 0.0, IMAGE( I, IY ) ) /
     :                     MAX( 1.0, ENERGY( I ) )
                     COUNT( IY_DELTA ) = COUNT( IY_DELTA ) + 1
                  END IF
               END IF
            END DO
         END IF

*     Normalise by number of pixels contributing this increment.
         IF ( COUNT( IY_DELTA ) .GT. 0 )
     :      EXPECTED_PERCENT( IY_DELTA ) =
     :            EXPECTED_PERCENT( IY_DELTA ) /
     :            FLOAT( COUNT( IY_DELTA ) )
      END DO

*  Renormalise expected percentages to balance for any missing increments.
      EXPCTOT = 0.0
      DO IY_DELTA = DEK_BELOW, DEK_ABOVE
         EXPCTOT = EXPCTOT + EXPECTED_PERCENT ( IY_DELTA )
      END DO
      DO IY_DELTA = DEK_BELOW, DEK_ABOVE
         EXPECTED_PERCENT( IY_DELTA ) =
     :         EXPECTED_PERCENT( IY_DELTA ) / MAX( 1.0, EXPCTOT )
      END DO

*   Initialise full order pixel counters.
      PIXEL_COUNT = 0
      TOTAL = 0.0
      TOTSQ = 0.0

*  Loop through increments from below to above order trace.
      DO IY_DELTA = DEK_BELOW, DEK_ABOVE

*     If processing increment this time, and pixel coords in range then.
         IF ( PROCESS( IY_DELTA ) ) THEN
            DO I = 1, NX
              IY = INT( Y_TRACE_COORD( I ) + 0.5 ) + IY_DELTA
              IF ( IY .GT. 0 .AND. IY .LE. NY ) THEN

*           If image pixel OK then.
               IF ( QUALITY( I, IY ) .EQ. 0 .AND.
     :              IMAGE( I, IY ) .NE. ECH__BAD_REAL .AND.
     :              ENERGY( I ) .NE. 0.0 ) THEN

*                 Add pixel and its ratio of expected/observed intensity
*                 to dataset.
                     PIXEL_COUNT = PIXEL_COUNT + 1
                     DATA( PIXEL_COUNT ) = ( MAX( 1., IMAGE( I, IY ) ) /
     :                     MAX( 1.0, ENERGY( I ) ) ) /
     :                     EXPECTED_PERCENT( IY_DELTA )

*                 Update totals for statistics.
                     TOTAL = TOTAL + DATA( PIXEL_COUNT )
                     TOTSQ = TOTSQ + DATA( PIXEL_COUNT ) *
     :                     DATA( PIXEL_COUNT )

*                 Keep record of pixels location on image.
                     INDEX_X( PIXEL_COUNT ) = I
                     INDEX_Y( PIXEL_COUNT ) = IY
                  END IF
               END IF
            END DO
         END IF
      END DO

*  Calculate mean and sigma of observed data set.
*  IE. We have a collected a set of expected/observed pixel value ratios
*  and calculated the mean and sigma of this dataset. For normal
*  data obeying gaussian statistics we can compare the observed
*  ratios to a calculated gaussian using the mean and sigma.
*  We then assume that the cosmic ray pixels have a higher observed
*  intensity than that predicted by their neighbours ie they
*  have a ratio > 1. We set a threshold at ratio > 1. + n*sigma
*  and clip at that point.
      MEAN = TOTAL / FLOAT( MAX( 1, PIXEL_COUNT ) )
      SIGMA = SQRT( ABS( TOTSQ / FLOAT( MAX( 1, PIXEL_COUNT ) ) -
     :      MEAN * MEAN ) )

      END
