      SUBROUTINE ECH_LOCATE_ORDERS(
     :           IMAGE,
     :           NX,
     :           NY,
     :           XBOX,
     :           USE_MEDIAN,
     :           AUTO_LOCATE,
     :           CHECK_PARTORD,
     :           ORDER_SLOPE,
     :           N_ORDERS,
     :           TRACE_WIDTH_THRESH,
     :           TRACE_WIDTH,
     :           ORDER_Y_POSITIONS,
     :           CONVOLVED,
     :           CONVOLVED2,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_LOCATE_ORDERS

*  Purpose:
*     Locate order candidates.

*  Description:
*     This routine locates the centres of the orders in an echellogram.
*     The central XBOX columns are then sampled using a box scaled
*     according to the expected order size.  A triangle function
*     of appropriate dimension is then convolved with the array and
*     the resluting array is smoothed.  A search is then made for the
*     local peak values respresenting the approximate order centers.
*
*     The central three columns of each order are examined and assumed
*     to be local peak intensities, the program then steps out above/below
*     each order until it reaches either: the next order, or a pixel
*     whose intensity falls below the threshold * peak intensity.
*     The 'threshold' being a tunable parameter (default .90 = 90%)

*  Invocation:
*     CALL ECH_LOCATE_ORDERS(
*     :    IMAGE,
*     :    NX,
*     :    NY,
*     :    XBOX,
*     :    USE_MEDIAN,
*     :    AUTO_LOCATE,
*     :    CHECK_PARTORD,
*     :    ORDER_SLOPE,
*     :    N_ORDERS,
*     :    TRACE_WIDTH_THRESH,
*     :    TRACE_WIDTH,
*     :    ORDER_Y_POSITIONS,
*     :    CONVOLVED,
*     :    CONVOLVED2,
*     :    STATUS
*     :   )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     XBOX = INTEGER (Given)
*        Sampling box size in x pixels.
*     USE_MEDIAN = LOGICAL (Given)
*        TRUE if median filtering is to be used.
*     AUTO_LOCATE = LOGICAL (Given)
*        TRUE if automatic order location is required.
*     CHECK_PARTORD = LOGICAL (Given)
*        TRUE if partial order checking.
*     ORDER_SLOPE = REAL (Given)
*        Estimated slope of orders across frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders found in echellogram.
*     TRACE_WIDTH_THRESH = INTEGER (Given)
*        Threshold for trace edge estimation.
*     TRACE_WIDTH = INTEGER (Returned)
*        Approx width of object profile in orders.
*     ORDER_Y_POSITIONS = INTEGER (Returned)
*        Row numbers of located orders.
*     CONVOLVED = REAL (Temporary Workspace)
*        Calculated functions.
*     CONVOLVED2 = REAL (Temporary Workspace)
*        Calculated functions.
*     STATUS = INTEGER (given and Returned)
*        Input/Ouput status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     21-MAY-1997 (MJC):
*       Tidy up.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      REAL IMAGE( NX, NY )
      INTEGER XBOX
      LOGICAL USE_MEDIAN
      LOGICAL AUTO_LOCATE
      LOGICAL CHECK_PARTORD
      REAL ORDER_SLOPE
      REAL TRACE_WIDTH_THRESH

*  Arguments Returned:
      INTEGER ORDER_Y_POSITIONS( N_ORDERS )
      INTEGER TRACE_WIDTH

*  Workspace:
      REAL CONVOLVED( NY )
      REAL CONVOLVED2( NY )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL VALUE_AT_PEAK

      INTEGER I
      INTEGER COUNT
      INTEGER IORD
      INTEGER NR_ORDERS
      INTEGER IY
      INTEGER IIX
      INTEGER AVERAGE_SEPARATION
      INTEGER IIY
      INTEGER MIN_SEP
      INTEGER LOWER_LIMIT
      INTEGER UPPER_LIMIT
      INTEGER NCHAR1
      INTEGER NCHAR2

      LOGICAL ADD_BELOW
      LOGICAL ADD_ABOVE

      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  If automatic order location is being tried.
      AVERAGE_SEPARATION = TRACE_WIDTH
      IF ( AUTO_LOCATE ) THEN

*     Convolve central region of frame and find peaks.
         CALL ECH_FIND_ORDER_PEAKS( IMAGE, NX, NY, XBOX, USE_MEDIAN,
     :        AVERAGE_SEPARATION,  N_ORDERS, ORDER_Y_POSITIONS,
     :        CONVOLVED, CONVOLVED2, NR_ORDERS, STATUS )

*  Else transfer user specified positions from graphics routine common
      ELSE
         DO I = 1, NUM_CURS_POSITIONS
            ORDER_Y_POSITIONS( I ) = INT( X_CURS_POSITIONS( I ) + 0.5 )
         END DO
         NR_ORDERS = NUM_CURS_POSITIONS
         NUM_CURS_POSITIONS = 0
      END IF

*  Check possible partial orders by using 'slope' and top/bottom
*  order position estimates.
      IF ( CHECK_PARTORD ) THEN
         ADD_BELOW = .FALSE.
         ADD_ABOVE = .FALSE.
         IF ( ABS( INT( ORDER_SLOPE * FLOAT( NX / 4 ) ) ) .GT. 5 ) THEN
            IF ( N_ORDERS .GT. 1 ) THEN
               IF ( FLOAT( 2 * ORDER_Y_POSITIONS( 1 ) -
     :                         ORDER_Y_POSITIONS( 2 ) ) +
     :              FLOAT( NX / 4 ) * ORDER_SLOPE .GT. 5.0 ) THEN
                  ADD_BELOW = .TRUE.
               END IF
               IF ( FLOAT( 2 * ORDER_Y_POSITIONS( NR_ORDERS ) -
     :                         ORDER_Y_POSITIONS( NR_ORDERS - 1 ) ) +
     :              FLOAT( NX / 4 ) * ORDER_SLOPE .LT.
     :              FLOAT( NY - 5 ) ) THEN
                  ADD_ABOVE = .TRUE.
               END IF
               IF ( ADD_BELOW ) THEN
                  DO IORD = NR_ORDERS + 1, 2, -1
                     ORDER_Y_POSITIONS( IORD ) =
     :                     ORDER_Y_POSITIONS( IORD - 1 )
                  END DO
                  ORDER_Y_POSITIONS( 1 ) = 2 * ORDER_Y_POSITIONS( 2 ) -
     :                  ORDER_Y_POSITIONS( 3 )
                  NR_ORDERS = NR_ORDERS + 1
               END IF
               IF ( ADD_ABOVE ) THEN
                  ORDER_Y_POSITIONS( NR_ORDERS + 1 ) =
     :                  2 * ORDER_Y_POSITIONS( NR_ORDERS ) -
     :                  ORDER_Y_POSITIONS( NR_ORDERS - 1 )
                  NR_ORDERS = NR_ORDERS + 1
               END IF
            END IF
         END IF

*     Check for partial orders parallel to detector rows.
         IF ( NR_ORDERS .GT. 3 ) THEN
            MIN_SEP = 2 * AVERAGE_SEPARATION / 3
            IF ( .NOT. ADD_BELOW ) THEN
               IF ( ORDER_Y_POSITIONS( 2 ) - ORDER_Y_POSITIONS( 1 ) .GE.
     :              MIN_SEP .AND.
     :              ORDER_Y_POSITIONS( 1 ) .GT. MIN_SEP  ) THEN
                  DO IORD = NR_ORDERS + 1, 2, -1
                     ORDER_Y_POSITIONS( IORD ) =
     :                     ORDER_Y_POSITIONS( IORD - 1 )
                  END DO
                  NR_ORDERS = NR_ORDERS + 1
                  ORDER_Y_POSITIONS( 1 ) = 2 * ORDER_Y_POSITIONS( 2 ) -
     :                  ORDER_Y_POSITIONS( 3 )
               END IF
            END IF
            IF ( .NOT. ADD_ABOVE ) THEN
               IF ( ORDER_Y_POSITIONS( NR_ORDERS - 1 ) -
     :              ORDER_Y_POSITIONS( NR_ORDERS - 2 ) .GE.
     :              MIN_SEP .AND. NY -
     :              ORDER_Y_POSITIONS( NR_ORDERS ) .GT. MIN_SEP  ) THEN
                  ORDER_Y_POSITIONS( NR_ORDERS + 1 ) =
     :                  2 * ORDER_Y_POSITIONS( NR_ORDERS ) -
     :                  ORDER_Y_POSITIONS( NR_ORDERS - 1 )
                  NR_ORDERS = NR_ORDERS + 1
               END IF
            END IF
         END IF
      END IF

*  Exit now if no orders found.
      IF ( NR_ORDERS .EQ. 0 ) RETURN

*  Output approx order Y positions if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_INFO ) .GT. 0 ) THEN
         DO I = 1, NR_ORDERS
            CALL CHR_ITOC( I, REF_STR1, NCHAR1 )
            CALL CHR_ITOC( ORDER_Y_POSITIONS( I ), REF_STR2, NCHAR2 )
            REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :            ', centre at row ' // REF_STR2( :NCHAR2 ) // '.'
            CALL ECH_REPORT( ECH__TXT_INFO, REPORT_STRING )
         END DO
      END IF

*  Estimate order size to be used as a box size for tracing.
      COUNT = 0
      TRACE_WIDTH = 0

*  Loop through located orders.
      DO IORD = 1, NR_ORDERS
         IY = ORDER_Y_POSITIONS( IORD )
         IF ( IY .GT. 0 .AND. IY .LE. NY ) THEN

*        Loop through central three columns.
            DO IIX = -1, 1

*           Get order peak (assumed) pixel value.
               VALUE_AT_PEAK = MAX( IMAGE( NX / 2 + IIX, IY ), 0.0 )

*           If reasonable (>0) value find object edges (approx).
               IF ( VALUE_AT_PEAK .GT. 0.0 ) THEN
                  UPPER_LIMIT = 0
                  LOWER_LIMIT = 0

*              Loop stepping out from order towards top/bottom of frame.
                  DO IIY = 1, AVERAGE_SEPARATION / 2 + 1

*                 If upper edge not yet found, and still in frame.
                     IF ( UPPER_LIMIT .EQ. 0 ) THEN
                        IF ( IY + IIY .GT. 0 .AND. IY + IIY .LE. NY )
     :                       THEN

*                       If intensity has dropped below peak intensity times
*                       threshold value store upper limit.
                           IF ( IMAGE( NX / 2 + IIX, IY + IIY ) .GE. 0.0
     :                          .AND. IMAGE( NX / 2 + IIX, IY + IIY )
     :                          .LE. VALUE_AT_PEAK * TRACE_WIDTH_THRESH
     :                          ) THEN
                              UPPER_LIMIT = IIY
                           END IF
                        END IF
                     END IF

*                 If lower edge not yet found, and still in frame.
                     IF ( LOWER_LIMIT .EQ. 0 ) THEN
                        IF ( IY - IIY .GT. 0 .AND. IY - IIY .LE. NY )
     :                       THEN

*                    If intensity has dropped below peak intensity times
*                    threshold value store lower limit.
                           IF ( IMAGE( NX / 2 + IIX, IY - IIY ) .GE. 0.0
     :                          .AND. IMAGE( NX / 2 + IIX, IY - IIY )
     :                          .LE. VALUE_AT_PEAK * TRACE_WIDTH_THRESH
     :                          ) THEN
                              LOWER_LIMIT = -IIY
                           END IF
                        END IF
                     END IF
                  END DO

*              If we found both an upper and lower limit for this order
*              column combination then add width estimate to sum of
*              estimates.
                  IF ( UPPER_LIMIT .NE. 0 .AND. LOWER_LIMIT .NE. 0 )
     :                 THEN
                      COUNT = COUNT + 1
                      TRACE_WIDTH = TRACE_WIDTH +
     :                      UPPER_LIMIT - LOWER_LIMIT + 1
                  END IF
               END IF
            END DO
         END IF
      END DO

*  If we found any estimates at all, work out the average value.
      IF ( COUNT .GT. 0 ) THEN
         TRACE_WIDTH = INT( FLOAT( TRACE_WIDTH ) / FLOAT( COUNT ) )
         STATUS = 0
         IF ( IAND( REPORT_MODE, RPM_FULL + RPM_INFO ) .GT. 0 ) THEN
             CALL CHR_ITOC( TRACE_WIDTH, REF_STR1, NCHAR1 )
             REPORT_STRING = ' Trace width estimated at ' //
     :             REF_STR1( :NCHAR1 ) // ' pixels.'
             CALL ECH_REPORT( ECH__TXT_INFO, REPORT_STRING )
         END IF

*  Otherwise default the trace width estimate to the average order
*  separation.
      ELSE
         TRACE_WIDTH = AVERAGE_SEPARATION
         CALL ECH_SET_CONTEXT( 'PROBLEM', 'No trace width' )
         IF ( IAND( REPORT_MODE, RPM_FULL +
     :        RPM_INFO + RPM_ERROR ) .GT. 0 ) THEN
             CALL ECH_REPORT( 0, ' Could not estimate width of trace.' )
             CALL CHR_ITOC( TRACE_WIDTH, REF_STR1, NCHAR1 )
             REPORT_STRING = ' Using average order separation of ' //
     :            REF_STR1( :NCHAR1 ) // ' pixels.'
             CALL ECH_REPORT( ECH__TXT_INFO, REPORT_STRING )
         END IF
         STATUS = ECH__NO_TRACEWID
      END IF

      TRACE_WIDTH = MIN( TRACE_WIDTH, 100 )

      END
