      SUBROUTINE ECH_COUNT_ORDERS(
     :           IMAGE,
     :           NX,
     :           NY,
     :           XBOX,
     :           USE_MEDIAN,
     :           AUTO_LOCATE,
     :           CHECK_PARTORD,
     :           ORDER_SLOPE,
     :           AVERAGE_SEPARATION,
     :           NR_ORDERS,
     :           CONVOLVED,
     :           CONVOLVED2,
     :           CORRELATE,
     :           CORREL_COUNT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_COUNT_ORDERS

*  Purpose:
*     Count number of orders in echellogram.

*  Description:
*     The method used is to first collapse or median filter the central
*     20 columns of the input frame into a 1-D array, and then apply a
*     triangle filter to this array.  The autocorrelation function is then
*     calculated and the peak (corresponding to average order seperation)
*     is located.  The central columns are then resampled using a box
*     scaled according to the expected order size.  A triangle function
*     of appropriate dimension is then convolved with the array and
*     the resluting array is smoothed.  A search is then made for the
*     local peak values respresenting the approximate order centers.

*  Invocation:
*     CALL ECH_COUNT_ORDERS(
*    :     IMAGE,
*    :     NX,
*    :     NY,
*    :     XBOX,
*    :     USE_MEDIAN,
*    :     AUTO_LOCATE,
*    :     CHECK_PARTORD,
*    :     ORDER_SLOPE,
*    :     AVERAGE_SEPARATION,
*    :     NR_ORDERS,
*    :     CONVOLVED,
*    :     CONVOLVED2,
*    :     CORRELATE,
*    :     CORREL_COUNT,
*    :     STATUS
*    :    )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions NX columns and NY rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     XBOX = INTEGER (Given)
*        Sampling box size in X-pixels.
*     USE_MEDIAN = LOGICAL (Given)
*        TRUE if median filtering is to be used.
*     AUTO_LOCATE = LOGICAL (Given)
*        TRUE if automatic order location is required.
*     CHECK_PARTORD = LOGICAL (Given)
*        TRUE if automatic checking for partial orders.
*     ORDER_SLOPE = REAL (Given)
*        Estimated slope of orders across frame.
*     AVERAGE_SEPARATION = REAL (Returned)
*        Average_separation.
*     NR_ORDERS = INTEGER (Returned)
*        Number of orders found in echellogram.
*     CONVOLVED = REAL (Temporary Workspace)
*        Calculated functions.
*     CONVOLVED2 = REAL (Temporary Workspace)
*        Calculated functions.
*     CORRELATE = REAL (Temporary Workspace)
*        Calculated autocorrelation function.
*     CORREL_COUNT = INTEGER (Temporary Workspace)
*        Counts for normalising autocorrelation.
*     STATUS = INTEGER (Given and Returned)
*        Input/Ouput status conditions.

*  Method:
*     Loop through all rows of input frame summing within the sampling
*     box, and convolving with a 5 pixel triangle function
*        Loop through pixels in sample box and convolve summed row values
*        with the triangle function
*         If any good pixels, convolve sum with triangle filter
*        Endloop
*     End loop
*     Calculate autocorrelation function for all pixel seperations
*     up to one third the y dimension of the input frame.
*     Normalise autocorrelation
*     Smooth autocorrelation
*     Loop through autocorrelation array
*         If this is a local peak, and larger than any previous peak in
*         magnitude, then check its independence
*             If first peak found then remember its position
*             Else check its independence by insisting that its position
*             is not within +/- 1.5 pixel-units of a multiple of the previously
*             selected peak.
*              New peak found, remember where
*             Endif
*         Endif
*     End loop
*     Convolve central region of frame and find peaks
*     plot up a graph of the convolved central section and let the
*     user select the order centres with a cursor
*     Plot graph and loop gathering cursor positions until the user
*     types a 'Q' to finish
*     Check possible partial orders by using 'slope' and top/bottom
*     order position estimates
*     Check for partial orders parallel to detector rows
*     Setup result for caller
*     Setup the parameter NUM_ORDERS to reflect the number found

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     18-JUL-1996 (MJC):
*       Prologue, tidy.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_CONTEXT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_GRAPHICS.INC'

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

*  Arguments Returned:
      INTEGER AVERAGE_SEPARATION

*  Workspace:
      REAL CONVOLVED( NY )
      REAL CONVOLVED2( NY )
      REAL CORRELATE( NY )
      INTEGER CORREL_COUNT( NY )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER TRI_HWID
      PARAMETER ( TRI_HWID = 2 )

*  Local Variables:
      REAL PLOT_ARRAY( 5000 )
      REAL TRIANGLE( - TRI_HWID : TRI_HWID )
      REAL CORR_MAX
      REAL SUM
      REAL XP
      REAL YP
      REAL YSC
      REAL YSL

      INTEGER ORDER_Y_POSITIONS( MAX_ALLOWED_ORDERS )
      INTEGER I
      INTEGER XSTRT
      INTEGER ISCALE
      INTEGER NR_ORDERS
      INTEGER IY
      INTEGER IIY
      INTEGER YINDEX
      INTEGER MIN_SEP
      INTEGER MINDIST
      INTEGER NEAREST
      INTEGER OPTIONS
      INTEGER DUMMY
      INTEGER IORD
      INTEGER NCHAR1

      LOGICAL ACCEPTED
      LOGICAL REFRESH
      LOGICAL ADD_BELOW
      LOGICAL ADD_ABOVE
      LOGICAL MENU

      CHARACTER*8 REF_STR1
      CHARACTER*3 CNUM

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) )  RETURN

*  Will have a full menu first time.
      MENU = .TRUE.

*  Define limits of sampling box.
      XSTRT = MAX( 1, NX / 2 - XBOX / 2 )
      XBOX = MIN( XBOX, NX )
      IF ( USE_MEDIAN ) THEN
         DO I = - TRI_HWID, TRI_HWID
            TRIANGLE( I ) = 0.0
         END DO
         TRIANGLE( 0 ) = 1.0

      ELSE
         DO I = - TRI_HWID, TRI_HWID
            TRIANGLE( I ) = 1.0 / MAX( 2.0 * ABS( FLOAT( I ) ), 1.0 )
         END DO
      END IF
      CALL ECH_ZERO_REAL( NY, CONVOLVED( 1 ) )
      CALL ECH_ZERO_REAL( NY, CONVOLVED2( 1 ) )
      CALL ECH_ZERO_REAL( NY, CORRELATE( 1 ) )
      CALL ECH_ZERO_REAL( NY, CORREL_COUNT( 1 ) )

*  Loop through all rows of input frame summing within the sampling
*  box, and convolving with a 5-pixel triangle function.
      DO IY = 1 + TRI_HWID, NY - TRI_HWID

*     Loop through pixels in sample box and convolve summed row values
*     with the triangle function.
         DO IIY = - TRI_HWID, TRI_HWID
            YINDEX = IY + IIY
            SUM = 0.0
            CALL ECH_MEAN_MEDIAN( XBOX, IMAGE( XSTRT, YINDEX ),
     :           USE_MEDIAN, .FALSE., SUM, STATUS )

*        If any good pixels, convolve sum with triangle filter
            IF ( SUM .GT. 0.0 ) THEN
               CONVOLVED2( IY ) = CONVOLVED2( IY ) + SUM *
     :               TRIANGLE( IIY )
            END IF
         END DO
         PLOT_ARRAY( IY ) = CONVOLVED2( IY )
      END DO

      DO I = 1, TRI_HWID
         PLOT_ARRAY( I ) = PLOT_ARRAY( TRI_HWID + 1 )
         PLOT_ARRAY( NY - I + 1 ) = PLOT_ARRAY( NY - TRI_HWID )
      END DO

*  Calculate autocorrelation function for all pixel seperations
*  up to one third the y dimension of the input frame.
      DO IY = 1, NY
         DO IIY = 2, NY / 3
            IF ( IY + IIY + 1 .LE. NY ) THEN
               CORRELATE( IIY ) = CORRELATE( IIY ) +
     :           ( CONVOLVED2( IY + 1 ) - CONVOLVED2( IY ) ) *
     :           ( CONVOLVED2( IY + IIY + 1 ) - CONVOLVED2( IY + IIY ) )
               CORREL_COUNT( IIY ) = CORREL_COUNT( IIY ) + 1
            END IF
         END DO
      END DO

*  Normalise autocorrelation.
      DO ISCALE = 2, NY / 3
         CORRELATE( ISCALE ) = CORRELATE( ISCALE ) /
     :         FLOAT( CORREL_COUNT( ISCALE ) )
      END DO

*  Smooth autocorrelation.
      DO I = 1, 5
         DO ISCALE = 4, NY / 3 - 3
            CONVOLVED( ISCALE ) = ( CORRELATE( ISCALE ) +
     :            CORRELATE( ISCALE - 1 ) +
     :            CORRELATE( ISCALE + 1 ) ) / 3.0
         END DO
         DO ISCALE = 4, NY / 3 - 3
            CORRELATE( ISCALE ) = CONVOLVED( ISCALE )
         END DO
      END DO

*  Search autocorrelation function for highest (independent) peak.
      CORR_MAX = 0.0
      AVERAGE_SEPARATION = 0

*  Loop through autocorrelation array.
      DO ISCALE = 4, NY / 3

*     If this is a local peak, and larger than any previous peak in
*     magnitude, then check its independence.
         IF ( CORRELATE( ISCALE ) .GT. CORRELATE( ISCALE - 1 ) .AND.
     :        CORRELATE( ISCALE + 1 ) .GT. CORRELATE( ISCALE + 2 ) .AND.
     :        CORRELATE( ISCALE - 1 ) .GT. CORRELATE( ISCALE - 2 ) .AND.
     :        CORRELATE( ISCALE + 2 ) .GT. CORRELATE( ISCALE + 3 ) .AND.
     :        CORRELATE( ISCALE - 2 ) .GT. CORRELATE( ISCALE - 3 ) .AND.
     :        CORRELATE( ISCALE ) .GT. CORRELATE( ISCALE + 1 ) ) THEN
            IF ( CORRELATE( ISCALE ) .GT. CORR_MAX ) THEN

*          If first peak found then remember its position.
               IF ( AVERAGE_SEPARATION .EQ. 0 ) THEN
                  CORR_MAX = CORRELATE( ISCALE )
                  AVERAGE_SEPARATION = ISCALE

*          Else check its independence by insisting that its position
*          is not within +/- 1.5 pixel-units of a multiple of the
*          previously selected peak.
               ELSE IF ( ABS( FLOAT( INT ( FLOAT( ISCALE ) /
     :                   FLOAT( AVERAGE_SEPARATION ) + 0.5 ) ) -
     :                   FLOAT( ISCALE ) / FLOAT( AVERAGE_SEPARATION ) )
     :                   .GT. 1.5 / FLOAT( AVERAGE_SEPARATION ) ) THEN

*             New peak found, remember where.
                  CORR_MAX = CORRELATE( ISCALE )
                  AVERAGE_SEPARATION = ISCALE
               END IF
            END IF
         END IF
      END DO

*  Convolve central region of frame and find peaks.
      IF ( AVERAGE_SEPARATION .EQ. 0 ) THEN
         AVERAGE_SEPARATION = NY / 3
      END IF
      N_ORDERS = MAX_ALLOWED_ORDERS
      CALL ECH_FIND_ORDER_PEAKS(
     :     IMAGE,
     :     NX,
     :     NY,
     :     XBOX,
     :     USE_MEDIAN,
     :     AVERAGE_SEPARATION,
     :     N_ORDERS,
     :     ORDER_Y_POSITIONS,
     :     CONVOLVED,
     :     CONVOLVED2,
     :     NR_ORDERS,
     :     STATUS
     :    )

*  Plot up a graph of the convolved central section and let the
*  user select the order centres with a cursor.
      USER_INPUT_CHAR = 'Q'
      NUM_CURS_POSITIONS = 0
      OPTIONS = GRPH_CALC_MINMAX + GRPH_GEN_XAXIS
      ACCEPTED = .FALSE.
      REFRESH = .TRUE.

*  Plot graph and loop gathering cursor positions until the user
*  types an 'E' to finish.
      YSC = 0.0
      YSL = 1.0E20
      DO I = NY, 1, -1
         IF ( PLOT_ARRAY( I ) .GT. YSC ) YSC = PLOT_ARRAY( I )
         IF ( PLOT_ARRAY( I ) .LT. YSL ) YSL = PLOT_ARRAY( I )
      END DO
      DO WHILE ( .NOT. ACCEPTED )
         IF ( REFRESH ) THEN
            CALL ECH_PLOT_GRAPH(
     :           ny, dummy, plot_array, 0, 0, 0, 0,
     :           'Pixel row number', 'Intensity',
     :           'Use cursor to select and Add/Delete',
     :           0, 0, options, 'LINES', status )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DO I = 1, NR_ORDERS
                  XP = FLOAT( ORDER_Y_POSITIONS( I ) )
                  YP = YSL + 0.5 * ( YSC - YSL )
                  CALL CHR_ITOC( I, CNUM, NCHAR1 )
                  CALL ECH_GR_SET_COLOUR( COL_RED )
                  CALL PGPTXT( XP, YP, 0.0, 0.0, CNUM )
                  CALL PGMOVE( XP, YSL + 0.2 * ( YSC - YSL ) )
                  CALL PGDRAW( XP, YSL + 0.8 * ( YSC - YSL ) )
               END DO
            END IF
            CALL ECH_GR_SET_COLOUR( COL_BLACK )
         END IF
         CALL CHR_ITOC( NR_ORDERS, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Current number of orders: ' //
     :         REF_STR1( :NCHAR1 ) // '.'

         IF ( AUTO_LOCATE ) THEN
            USER_INPUT_CHAR = 'E'

         ELSE
  100       CONTINUE
            IF ( MENU ) THEN
               CALL ECH_REPORT( 0, REPORT_STRING )
               CALL ECH_REPORT( 0, ' Options:' )
               CALL ECH_REPORT( 0, '   A - Add an order.')
               CALL ECH_REPORT( 0, '   D - Delete an order.')
               CALL ECH_REPORT( 0, '   C - Clear (delete all orders).')
               CALL ECH_REPORT( 0, '   R - Re-plot.')
               CALL ECH_REPORT( 0, '   E - Exit.')
               CALL ECH_REPORT( 0, '   M - Full menu display.' )
               MENU = .FALSE.

            ELSE
               CALL ECH_REPORT( 0, REPORT_STRING )
               CALL ECH_REPORT( 0, ' Options [ A D C R E M ]' )
            END IF
            CALL ECH_READ_GRPH_CURSOR( STATUS )

*        Find nearest order.
            NEAREST = 0
            MINDIST = 1000000
            DO I = 1, NR_ORDERS
               IF ( ABS ( ORDER_Y_POSITIONS( I ) - INT( X_CURSOR ) )
     :              .LT. MINDIST ) THEN
                  MINDIST = ABS( ORDER_Y_POSITIONS( I ) -
     :                      INT( X_CURSOR ) )
                  NEAREST = I
               END IF
            END DO
         END IF

*     Option 'M': Display the full menu.
         IF ( USER_INPUT_CHAR .EQ. 'M' ) THEN
            MENU = .TRUE.
            GO TO 100

*     Option 'A': Add an order.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'A' ) THEN
            IF ( nr_orders .GT. 0 ) THEN
               IF ( INT( X_CURSOR ) .GT. ORDER_Y_POSITIONS( NEAREST ) )
     :            NEAREST = NEAREST + 1
               DO I = NR_ORDERS + 1, NEAREST + 1, -1
                  ORDER_Y_POSITIONS( I ) = ORDER_Y_POSITIONS( I - 1 )
               END DO

            ELSE
               NEAREST = 1
            END IF
            ORDER_Y_POSITIONS( NEAREST ) = INT( X_CURSOR )
            NR_ORDERS = NR_ORDERS + 1
            CALL CHR_ITOC( INT( X_CURSOR ), REF_STR1, NCHAR1 )
            REPORT_STRING = ' Added order centered near row ' //
     :            REF_STR1( :NCHAR1 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            XP = X_CURSOR
            YP = YSL + .5 * ( YSC - YSL )
            CALL CHR_ITOC( NR_ORDERS, CNUM, NCHAR1 )
            CALL ECH_GR_SET_COLOUR( COL_GREEN )
            CALL PGPTXT( XP, YP, 0., 0., CNUM )
            CALL ECH_GR_SET_COLOUR( COL_RED )
            CALL PGMOVE( XP, YSL + 0.2 * ( YSC - YSL ) )
            CALL PGDRAW( XP, YSL + 0.8 * ( YSC - YSL ) )
            CALL ECH_GR_SET_COLOUR( COL_BLACK )
            REFRESH = .FALSE.

*     Option 'D': Delete an order.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'D' ) THEN
            IF ( NR_ORDERS .GT. 0 ) THEN
               DO I = NEAREST, NR_ORDERS
                  ORDER_Y_POSITIONS( I ) = ORDER_Y_POSITIONS( I + 1 )
               END DO
               ORDER_Y_POSITIONS( NR_ORDERS ) = 0
               NR_ORDERS = NR_ORDERS - 1
               CALL CHR_ITOC( INT( X_CURSOR ), REF_STR1, NCHAR1 )
               REPORT_STRING = ' Deleted order centered near row ' //
     :               REF_STR1( :NCHAR1 ) // '.'
               CALL ECH_REPORT( 0, REPORT_STRING )
               REFRESH = .TRUE.

            ELSE
               CALL ECH_REPORT( 0, ' No orders left to delete.' )
               REFRESH = .FALSE.
            END IF

*     Option 'C': Clear all orders.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'C' ) THEN
            DO I = NEAREST, NR_ORDERS
               ORDER_Y_POSITIONS( I ) = 0
            END DO
            NR_ORDERS = 0
            CALL ECH_REPORT( 0, ' Deleted all orders.' )
            REFRESH = .TRUE.

*     Option 'E': Exit.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'E' ) THEN
            ACCEPTED = .TRUE.

*     Option 'R': Refresh display.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'R' ) THEN
            REFRESH = .TRUE.

*     Unknown option.
         ELSE
            REPORT_STRING = ' Unknown Option: "' //
     :           USER_INPUT_CHAR // '".'
            CALL ECH_REPORT( 0, REPORT_STRING )
            GO TO 100
         END IF
      END DO

      NUM_CURS_POSITIONS = NR_ORDERS
      DO I = NR_ORDERS, 1, -1
         X_CURS_POSITIONS( I ) = FLOAT( ORDER_Y_POSITIONS( I ) )
      END DO

      IF ( NR_ORDERS .LT. 2 ) THEN
         AVERAGE_SEPARATION = NY / 3

      ELSE
         DO I = 2, NR_ORDERS
            AVERAGE_SEPARATION = AVERAGE_SEPARATION +
     :            INT( X_CURS_POSITIONS( I ) -
     :            X_CURS_POSITIONS( I - 1 ) )
         END DO
         AVERAGE_SEPARATION = AVERAGE_SEPARATION / NR_ORDERS
      END IF

      IF ( check_partord ) THEN

*  Check possible partial orders by using 'slope' and top/bottom
*  order position estimates.
      IF ( nr_orders .LT. max_allowed_orders-2 ) THEN
      add_below = .FALSE.
      add_above = .FALSE.
      IF ( ABS ( INT ( order_slope * FLOAT ( nx / 4 ) ) ).GT. 5 ) THEN
         IF ( nr_orders .GT. 1 ) THEN
          IF ( FLOAT ( order_y_positions ( 1 ) -
     :        ( order_y_positions ( 2 ) - order_y_positions ( 1 )) ) +
     :        FLOAT ( nx/4 ) * order_slope   .GT. 5. ) THEN
            add_below = .TRUE.
          END IF
          IF ( FLOAT ( order_y_positions ( 1 ) -
     :        ( order_y_positions ( 2 ) - order_y_positions ( 1 )) ) -
     :        FLOAT ( nx/4 ) * order_slope   .GT. 5. )  THEN
            add_below = .TRUE.
          END IF
          IF ( FLOAT ( order_y_positions ( nr_orders ) +
     :        ( order_y_positions ( nr_orders ) -
     :          order_y_positions ( nr_orders-1 ) ) ) +
     :        FLOAT ( nx/4 ) * order_slope    .LT. FLOAT(ny - 5) ) THEN
            add_above = .TRUE.
          END IF
          IF ( FLOAT ( order_y_positions ( nr_orders ) +
     :        ( order_y_positions ( nr_orders ) -
     :          order_y_positions ( nr_orders-1 ) )  ) -
     :        FLOAT ( nx/4 ) * order_slope  .LT. FLOAT(ny - 5) ) THEN
            add_above = .TRUE.
          END IF
          IF ( add_below ) THEN
            REPORT_STRING =' Possible partial order at bottom of frame.'
            CALL ECH_REPORT( ECH__TXT_INFO, REPORT_STRING )
            DO iord = nr_orders+1, 2, -1
               order_y_positions ( iord ) = order_y_positions ( iord-1 )
            END DO
            order_y_positions ( 1 ) = order_y_positions ( 2 ) -
     :        ( order_y_positions ( 3 ) - order_y_positions ( 2 )  )
            nr_orders = nr_orders + 1
          END IF
          IF ( add_above ) THEN
            REPORT_STRING = ' Possible partial order at top of frame.'
            CALL ECH_REPORT( ECH__TXT_INFO, REPORT_STRING )
            order_y_positions ( nr_orders + 1 ) =
     :                               order_y_positions ( nr_orders ) +
     :                       ( order_y_positions ( nr_orders ) -
     :                         order_y_positions ( nr_orders-1 )  )
            nr_orders = nr_orders + 1
          END IF
         END IF
      END IF
      END IF

*  Check for partial orders parallel to detector rows.
      IF ( NR_ORDERS .GT. 3 .AND.
     :     NR_ORDERS .LT. MAX_ALLOWED_ORDERS ) THEN
         MIN_SEP = 2 * AVERAGE_SEPARATION / 3
         IF ( .NOT. ADD_BELOW ) THEN
            IF ( ORDER_Y_POSITIONS( 2 ) -
     :           ORDER_Y_POSITIONS( 1 ) .GE. MIN_SEP .AND.
     :           ORDER_Y_POSITIONS( 1 ) .GT. MIN_SEP ) THEN
               DO IORD = NR_ORDERS + 1, 2, -1
                  ORDER_Y_POSITIONS( IORD ) =
     :                  ORDER_Y_POSITIONS( IORD - 1 )
               END DO
               NR_ORDERS = NR_ORDERS + 1
               ORDER_Y_POSITIONS( 1 ) = ORDER_Y_POSITIONS( 2 ) -
     :               ( ORDER_Y_POSITIONS( 3 ) - ORDER_Y_POSITIONS( 2 ) )
               CALL ECH_REPORT( 0,
     :              ' Suspected partial order at bottom of frame.' )
            END IF
         END IF
         IF ( .NOT. ADD_ABOVE ) THEN
            IF ( ORDER_Y_POSITIONS( NR_ORDERS - 1 ) -
     :           ORDER_Y_POSITIONS( NR_ORDERS - 2 ) .GE. MIN_SEP .AND.
     :           NY - ORDER_Y_POSITIONS( NR_ORDERS ) .GT. MIN_SEP ) THEN
               ORDER_Y_POSITIONS( NR_ORDERS + 1 ) =
     :               ORDER_Y_POSITIONS( NR_ORDERS ) +
     :               ( ORDER_Y_POSITIONS( NR_ORDERS ) -
     :                 ORDER_Y_POSITIONS( NR_ORDERS - 1 ) )
               NR_ORDERS = NR_ORDERS + 1
               CALL ECH_REPORT( 0,
     :              ' Suspected partial order at top of frame.' )
            END IF
         END IF
      END IF
      END IF

*  Setup result for caller.
      IF ( NR_ORDERS .GT. 0 ) THEN
         STATUS = SAI__OK
         IF ( IAND ( REPORT_MODE, RPM_FULL + RPM_INFO ) .GT. 0 ) THEN
             IF ( NR_ORDERS .GT. 1 ) THEN
             CALL CHR_ITOC( NR_ORDERS, REF_STR1, NCHAR1 )
                REPORT_STRING = ' Counted ' // REF_STR1( :NCHAR1 ) //
     :                ' orders.'

             ELSE
                REPORT_STRING = ' Counted one order.'
             END IF
             CALL ECH_REPORT( ECH__TXT_INFO, REPORT_STRING )
         END IF

*  Setup the parameter NUM_ORDERS to reflect the number found.
         CALL ECH_SET_PARAMETER( 'NUM_ORDERS', 'INT',
     :        FLOAT( NR_ORDERS ), 0, ' ', STATUS )

      ELSE
         CALL ECH_SET_CONTEXT( 'PROBLEM', 'No orders' )
         IF ( IAND( REPORT_MODE, RPM_FULL +
     :        RPM_INFO + RPM_ERROR ) .GT. 0 ) THEN
             REPORT_STRING = ' Could not find any orders.'
             CALL ECH_REPORT( ECH__TXT_INFO, REPORT_STRING )
         END IF
         STATUS = ECH__NO_ORDERS
      END IF

      END
