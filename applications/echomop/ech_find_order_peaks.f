      SUBROUTINE ECH_FIND_ORDER_PEAKS(
     :           IMAGE,
     :           NX,
     :           NY,
     :           XBOX,
     :           USE_MEDIAN,
     :           AVERAGE_SEPARATION,
     :           N_ORDERS,
     :           ORDER_Y_POSITIONS,
     :           CONVOLVED,
     :           CONVOLVED2,
     :           NR_ORDERS,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_FIND_ORDER_PEAKS

*  Purpose:
*     Locate positions (in Y) of orders at X = nX / 2.

*  Description:
*     The central columns of the image are sampled using a box
*     scaled according to the expected order size.  A triangle
*     function of appropriate dimension is then convolved with
*     the array and the resulting array is smoothed.  A search
*     is then made for the local peak values respresenting the
*     approximate order centers.

*  Invocation:
*      CALL ECH_FIND_ORDER_PEAKS(
*     :     IMAGE,
*     :     NX,
*     :     NY,
*     :     XBOX,
*     :     USE_MEDIAN,
*     :     AVERAGE_SEPARATION,
*     :     N_ORDERS,
*     :     ORDER_Y_POSITIONS,
*     :     CONVOLVED,
*     :     CONVOLVED2,
*     :     NR_ORDERS,
*     :     STATUS
*     :    )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     XBOX = INTEGER (Given)
*        Sampling box size in X pixels.
*     USE_MEDIAN = LOGICAL (Given)
*        TRUE if median filtering is to be used.
*     AVERAGE_SEPARATION = LOGICAL (Returned)
*        Average order separation.
*     N_ORDERS = INTEGER (Returned)
*        Number of orders found in echellogram.
*     CONVOLVED = REAL (Temporary Workspace)
*        Calculated functions.
*     CONVOLVED2 = REAL (Temporary Workspace)
*        Calculated functions.
*     STATUS = INTEGER (Given and Returned)
*        Input/Ouput status conditions.
*     ORDER_Y_POSITIONS INTEGER (Given and Returned)
*        Array of estimated order positions at column nx/2.
*     NR_ORDERS = INTEGER (Given and Returned)
*        Number of orders found in echellogram.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     21-MAY-1997 (MJC):
*       Tidy up, added prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      REAL IMAGE( NX, NY )
      INTEGER XBOX
      LOGICAL USE_MEDIAN

*  Arguments Returned:
      INTEGER AVERAGE_SEPARATION

*  Workspace:
      REAL CONVOLVED( NY )
      REAL CONVOLVED2( NY )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL TRIANGLE( -20 : 20 )
      REAL BRIGHTEST_PEAK
      REAL LOW_THRESH
      REAL RESULT

      INTEGER ORDER_Y_POSITIONS( N_ORDERS )
      INTEGER COUNT
      INTEGER GCOUNT
      INTEGER I
      INTEGER IABOVE
      INTEGER IBELOW
      INTEGER II
      INTEGER IY
      INTEGER IIY
      INTEGER MAX_ORDER_SIZE
      INTEGER NCHAR1
      INTEGER NR_ORDERS
      INTEGER NX2
      INTEGER SMOOTH_FACTOR
      INTEGER XSTRT
      INTEGER YBOX
      INTEGER YINDEX

      CHARACTER*8 REF_STR1

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Validate average separation and set YBOX size.
      MAX_ORDER_SIZE = AVERAGE_SEPARATION * 8 / 10
      IF ( IAND( REPORT_MODE, RPM_FULL ) .GT. 0 ) THEN
         CALL CHR_ITOC( MAX_ORDER_SIZE, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Estimated average order-separation is ' //
     :         REF_STR1( :NCHAR1 ) // ' pixels.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF


*  If calculated maximum order size is too small (less than 7 pixels)
*  then we assume only one order is present and set the Y sampling
*  box dimension to one third the Y dimension of the frame.
      IF ( MAX_ORDER_SIZE .LT. 7 ) THEN
         MAX_ORDER_SIZE = NY / 3
         YBOX = MIN( 20, MAX_ORDER_SIZE )
         IF ( IAND( REPORT_MODE, RPM_FULL + RPM_WARN ) .GT. 0 ) THEN
            CALL CHR_ITOC( MAX_ORDER_SIZE, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Estimate too small, reset to ' //
     :            REF_STR1( :NCHAR1 ) // ' pixels.'
            CALL ECH_REPORT( ECH__TXT_INFO, REPORT_STRING )
         END IF

*  else If calculated maximum order size is large (greater than 20
*  pixels) then set a Y sampling box dimension of 20 pixels to
*  avoid wasting time.
      ELSE IF ( MAX_ORDER_SIZE .GT. 20 ) THEN
         YBOX = 20
         IF ( IAND ( REPORT_MODE, RPM_FULL + RPM_INFO ) .GT. 0 ) THEN
            CALL CHR_ITOC( YBOX, REF_STR1, NCHAR1 )
            REPORT_STRING =
     :            ' Setting max Y-sampling box dimension to ' //
     :            REF_STR1( :NCHAR1 ) // ' pixels.'
            CALL ECH_REPORT( ECH__TXT_INFO, REPORT_STRING )
         END IF

*  Otherwise set sampling box size to maximum order size.
      ELSE
         YBOX = MAX_ORDER_SIZE
      END IF

*  Commented out by MJC 28-MAR-1995.
*  Can't see why this is here, and worse, it causes a crash.
*      IF ( USE_MEDIAN ) YBOX = 1

*  Resample the central xbox columns of the frame, using either
*  mean or median and then convolve with a triangle function of
*  dimension YBOX.

*  Initialise workspace arrays, variables and triangle function.
      CALL ECH_ZERO_REAL( NY, CONVOLVED( 1 ) )
      CALL ECH_ZERO_REAL( NY, CONVOLVED2( 1 ) )
      NX2 = NX / 2
      XSTRT = MAX( 1, NX2 - XBOX / 2 )
      DO IY = -YBOX / 2, YBOX / 2
         TRIANGLE( IY ) = 1.0 - 1.5 * ABS( FLOAT( IY ) /
     :         FLOAT( YBOX ) )
         TRIANGLE( IY ) = TRIANGLE( IY ) * TRIANGLE( IY )
      END DO

*  Loop through each line of the frame.
      DO IY = 1 + YBOX / 2, NY - YBOX / 2
         IF ( USE_MEDIAN ) THEN
            CALL ECH_MEAN_MEDIAN( XBOX, IMAGE( XSTRT, IY ),
     :           USE_MEDIAN, .FALSE., CONVOLVED( IY ), STATUS )

         ELSE

*        Loop through rows of frame.
            DO IIY = -YBOX / 2, YBOX / 2
               YINDEX = IY + IIY
               RESULT = 0.0
               CALL ECH_MEAN_MEDIAN( XBOX, IMAGE( XSTRT, YINDEX ),
     :              USE_MEDIAN, .FALSE., RESULT, STATUS )

*           Convolve with triangle and add.
               IF ( RESULT .GT. 0 ) THEN
                  CONVOLVED( IY ) = CONVOLVED( IY ) +
     :                  RESULT * TRIANGLE( IIY )
               END IF
            END DO
         END IF
      END DO

*  Locally smooth the resulting convolved sample to try and
*  ensure that each order is represented by a slowly varying
*  single peaked distribution.
      IF ( .NOT. USE_MEDIAN ) THEN
         DO IY = 1, NY
            IBELOW = MAX( 1, IY - YBOX / 2 + 1 )
            IABOVE = MAX( 1, IY + YBOX / 2 - 1 )
            IF ( IABOVE .GT. NY ) IABOVE = IBELOW
            CONVOLVED2( IY ) = CONVOLVED( IY ) -
     :            ( CONVOLVED( IABOVE ) + CONVOLVED( IBELOW ) ) / 2.0
         END DO
         DO IY = 1, NY
            CONVOLVED( IY ) = CONVOLVED2( IY )
         END DO

      ELSE
         DO IY = 1, NY
            CONVOLVED2( IY ) = CONVOLVED( IY )
         END DO
      END IF

      SMOOTH_FACTOR = MIN( 10, MAX_ORDER_SIZE )
      IF ( USE_MEDIAN ) SMOOTH_FACTOR = 0
      DO I = 1, SMOOTH_FACTOR
         DO IY = 2, NY - 1
            CONVOLVED2( IY ) = ( CONVOLVED( IY - 1 ) +
     :            CONVOLVED( IY ) + CONVOLVED( IY + 1 ) ) / 3.0
         END DO
         DO IY = 2, NY - 1
            CONVOLVED( IY ) = CONVOLVED2( IY )
         END DO
      END DO

*  Record central column of image.
      DO IY = 1, NY
         CONVOLVED2( IY ) = MAX( IMAGE( NX2, IY ), 0.0 )
      END DO

*  Search for peak values in the smoothed array.
      COUNT = 0
      BRIGHTEST_PEAK = 0.0

*  Loop through each element (corresponds to a row in the frame).
      DO IY = 3, NY - 2

*     If this element is a local peak then.
         IF ( USE_MEDIAN .OR. ( CONVOLVED( IY - 2 ) .GT. 0.0 .AND.
     :        CONVOLVED( IY - 1 ) .GT. 0.0 .AND.
     :        CONVOLVED( IY ) .GT. 0.0 ) ) THEN
            IF ( CONVOLVED( IY - 1 ) .GT. CONVOLVED( IY ) .AND.
     :           CONVOLVED( IY - 1 ) .GT. CONVOLVED( IY - 2 ) ) THEN

*           If its NOT the first peak we've found.
               IF ( COUNT .GT. 0 .AND.
     :              COUNT .LT. MAX_ALLOWED_ORDERS ) THEN

*              If it is within 'max_order_size' from the
*              last local peak, AND has a greater value,
*              then ignore last peak and use current one
*              as a candidate instead.
                  IF ( IY - ORDER_Y_POSITIONS( COUNT ) .LT.
     :                 4 * YBOX / 5 ) THEN
                     IF ( CONVOLVED( ORDER_Y_POSITIONS( COUNT ) )
     :                    .LT. CONVOLVED( IY ) ) THEN
                        ORDER_Y_POSITIONS( COUNT ) = IY - 1
                        IF ( CONVOLVED2( ORDER_Y_POSITIONS( COUNT ) )
     :                       .GT. BRIGHTEST_PEAK )
     :                     BRIGHTEST_PEAK =
     :                       CONVOLVED2( ORDER_Y_POSITIONS( COUNT ) )
                     END IF

*              Otherwise its a good candidate, so remember it.
                  ELSE
                     COUNT = COUNT + 1
                     ORDER_Y_POSITIONS( COUNT ) = IY - 1
                     IF ( CONVOLVED2( ORDER_Y_POSITIONS( COUNT ) )
     :                    .GT. BRIGHTEST_PEAK )
     :                  BRIGHTEST_PEAK =
     :                      CONVOLVED2( ORDER_Y_POSITIONS( COUNT ) )
                  END IF

*           Else first peak candidate, so remember it.
               ELSE
                  COUNT = COUNT + 1
                  ORDER_Y_POSITIONS( COUNT ) = IY - 1
                  BRIGHTEST_PEAK =
     :                  CONVOLVED2( ORDER_Y_POSITIONS( COUNT ) )
               END IF
            END IF
         END IF
      END DO

*  Throw away any peaks which are less than 1% intensity of the brightest.
      LOW_THRESH = 0.01
      GCOUNT = 0
      I = 0
      DO WHILE ( I .LT. COUNT )
         I = I + 1
         IF ( ORDER_Y_POSITIONS( I ) .GT. 0 ) THEN
            IF ( CONVOLVED2( ORDER_Y_POSITIONS( I ) ) .LT.
     :           LOW_THRESH * BRIGHTEST_PEAK ) THEN
               II = I + 1
               DO II = I + 1, COUNT
                  ORDER_Y_POSITIONS( II - 1 ) = ORDER_Y_POSITIONS( II )
               END DO
               ORDER_Y_POSITIONS( II - 1 ) = 0
               I = I - 1

            ELSE
               GCOUNT = GCOUNT + 1
            END IF
         END IF
      END DO

*  Set number of order peaks found.
      NR_ORDERS = GCOUNT

      END
