      SUBROUTINE ECH_TRACE_CONSISTENCY(
     :           NX,
     :           NY,
     :           IMAGE,
     :           N_ORDERS,
     :           INTERACTIVE,
     :           CONSISTENCY_THRESH,
     :           RE_FIT_THRESH,
     :           MAXIMUM_SAMPLE,
     :           MAXIMUM_POLY,
     :           N_POLY,
     :           POLYNOMIALS,
     :           X_TRACE_COORD,
     :           TRACE_RE_FITS,
     :           FIT_WORK_XREAL,
     :           FIT_WORK_XDOUBLE,
     :           FIT_WORK_3XDOUBLE,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_TRACE_CONSISTENCY

*  Purpose:
*     Check order traces for consistency with each other.

*  Description:
*     This routine models the relationship between inter-order-centre gap
*     as a function of y position.  This model is re-evaluated at each
*     x coordinate, and the deviations of each fitted trace point are
*     evaluated.  Deviant points are replaced by the model predictions
*     and then the order is re-fitted with a new polynomial.

*  Invocation:
*     CALL ECH_TRACE_CONSISTENCY(
*     :    NX,
*     :    NY,
*     :    IMAGE,
*     :    N_ORDERS,
*     :    INTERACTIVE,
*     :    CONSISTENCY_THRESH,
*     :    RE_FIT_THRESH,
*     :    MAXIMUM_SAMPLE,
*     :    MAXIMUM_POLY,
*     :    N_POLY,
*     :    POLYNOMIALS,
*     :    X_TRACE_COORD,
*     :    TRACE_RE_FITS,
*     :    FIT_WORK_XREAL,
*     :    FIT_WORK_XDOUBLE,
*     :    FIT_WORK_3XDOUBLE,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     IMAGE = REAL( NX, NY ) (Given)
*        The traced image.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     CONSISTENCY_THRESH = REAL (Given)
*        Max permitted deviation on pixels between fits.
*     RE_FIT_THRESH = REAL (Given)
*        Maximum fraction of deviant points permitted before refit.
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if interactive mode.
*     MAXIMUM_SAMPLE = INTEGER (Given)
*        Maximum number of samples to be used in fit.
*     POLYNOMIALS = DOUBLE( MAXIMUM_POLY, N_ORDERS ) (Given and Returned)
*        Array of polynomial coefficients for each order.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Workspace for array of x-pixel coords.
*     TRACE_RE_FITS = DOUBLE (Temporary Workspace)
*        Workspace for array to-be-re-fitted y pixel coords.
*     FIT_WORK_XREAL = INTEGER (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     FIT_WORK_XDOUBLE = DOUBLE (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     FIT_WORK_3XDOUBLE = DOUBLE (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     N_POLY = INTEGER (Given)
*        Maximum degree of polynomial to attempt to fit.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If less than 3 orders give up now
*      Loop until no more changes necessary (ie orders all consistent)
*        Loop thru each x increment using order polynomials to predict
*        y coords.
*           Loop thru orders evaluating order specific polynomial
*            If a polynomial is NOT available then flag bad value
*            Else
*              Evaluate order polynomial at x increment, and x+1 increment
*              If x and x+1 samples are in a stable polynomial region then
*                 Save order number and y coordinate
*              Endif
*            Endif
*           End loop
*           Fit a polynomial thru the order-number/y-coordinates
*           Loop evaluating predicted y coordinates for each order at this increment
*              If a good coordinate was obtained then
*                 Calculate deviation from one fit to the other and update stats
*              Else use a fake (large) deviation
*              Endif
*              Update statistics
*           End loop
*        End loop
*        Calculate mean and standard deviation of differences
*        Loop thru x increments
*           Loop thru orders evaluating order specific polynomial
*            If a polynomial is NOT available then flag bad value
*            Else
*              Evaluate order polynomial at x increment
*              If x and x+1 samples are in a stable polynomial region then
*                 Save order number and y coordinate
*              Endif
*            Endif
*           End loop
*           Fit a polynomial thru the order-number/y-coordinates
*           Loop evaluating predicted y coordinates for each order at this increment
*              Record prediction for order
*              If a valid point obtained from original order polynomial then
*                 Calculate difference between the two fits at this increment
*                 If point is TOO deviant then use new prediction
*                 Else use original prediction
*                 Endif
*              Else use new prediction
*              Endif
*           End loop
*        End loop
*        Report details of deviation between the two types of fit
*        Determine 'worst' order
*         Loop reporting details for each order
*          If 'worst' order AND number of deviating points is significant then
*           Re-fit a polynomial for the order
*           Evaluate new fit and plot it
*            Re-Evaluate old fit and plot it over the top of the new one
*           If we are in user interactive mode then
*              Decide if user wants to accept/reject the new polynomial fit
*              If user wants to exit, set no order selected
*              Else if they choose to abandon this order, then ensure it
*                   is flagged as inactive
*              Else if they want to see the re-fit for a specific order
*              Else if they wnat to see the re-fit for a specific order
*              Else if - then alter degree of inter-order fit
*              Else if + then alter degree of inter-order fit
*              Else (default) we keep the previous fit
*              Endif
*           Else set modification required flag by default
*           Endif
*           If order polynomial is to be replaced by the new one, then
*              Zero all coefficients
*              Copy new coefficients
*           Endif
*          Endif
*         End loop
*      Endloop
*     Endif

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_GRAPHICS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      INTEGER N_ORDERS
      INTEGER N_POLY
      INTEGER MAXIMUM_POLY
      INTEGER MAXIMUM_SAMPLE
      REAL CONSISTENCY_THRESH
      REAL RE_FIT_THRESH
      DOUBLE PRECISION POLYNOMIALS( MAXIMUM_POLY, N_ORDERS )
      LOGICAL INTERACTIVE

*  Workspace:
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION TRACE_RE_FITS( NX, N_ORDERS )
      REAL FIT_WORK_XREAL( NX )
      DOUBLE PRECISION FIT_WORK_XDOUBLE( NX )
      DOUBLE PRECISION FIT_WORK_3XDOUBLE( 3 * NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION O_NUMBER( MAX_ALLOWED_ORDERS ) ! Order numbers.
      DOUBLE PRECISION Y_COORDS( MAX_ALLOWED_ORDERS )
*          ! Y coordinates of order centres.
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )
      DOUBLE PRECISION FIT2_COORD

      REAL MEAN_DEVIATION( MAX_ALLOWED_ORDERS )
*          ! Mean of all deviations.
      REAL MEANSQ_DEVIATION( MAX_ALLOWED_ORDERS )
*          ! Mean squared of all deviations.
      REAL SIGMA_DEVIATION( MAX_ALLOWED_ORDERS )
*          ! Standard deviation of deviations.
      REAL FIT_COORD( MAX_ALLOWED_ORDERS )
*          ! Fitted y coords using x polynomials.
      REAL GUESS( MAX_ALLOWED_ORDERS )
      REAL X_GRAPH( 2500 )
      REAL Y_GRAPH( 2500 )
      REAL THRESH
      REAL VALUE
      REAL DEVIATION
      REAL MAX_DEV
      REAL NEXT

      INTEGER SAMPLE_COUNT( MAX_ALLOWED_ORDERS )
*          ! Count of contributors to sigma estimate.
      INTEGER DEV_COUNT( MAX_ALLOWED_ORDERS )
*          ! Count of 'too' deviant points.
      INTEGER RE_FIT_COUNT( MAX_ALLOWED_ORDERS )
*          ! Count of re-fits per order.
      INTEGER NWCOEFF( MAX_ALLOWED_ORDERS )
*          ! Actual number of poly coeffs per order.
      INTEGER I
      INTEGER II
      INTEGER IORD
      INTEGER OPTIONS
      INTEGER X_STEP
      INTEGER XCOUNT
      INTEGER POLY_DEGREE
      INTEGER OLD_POLY_DEGREE
      INTEGER COUNT
      INTEGER IPOLY
      INTEGER TEMP_POLY
      INTEGER WORST
      INTEGER SELECTED
      INTEGER NREJ

      LOGICAL GLOBAL_PLOT
      LOGICAL MODIFY
      LOGICAL MODIFIED

      CHARACTER*64 TITLE

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  If less than 3 orders we can't do anything so exit.
      IF ( N_ORDERS .LT. 3 ) THEN
         GO TO 999
      END IF

      IPOLY = 3
      DO IORD = 1, N_ORDERS
         RE_FIT_COUNT( IORD ) = 0
         DO I = 1, MAXIMUM_POLY
            IF ( POLYNOMIALS( I, IORD ) .NE. 0.0 .AND.
     :           POLYNOMIALS( I, IORD ) .NE. ECH__BAD_REAL ) THEN
               NWCOEFF( IORD ) = I
            END IF
         END DO
         IF ( NWCOEFF( IORD ) .GT. 15 ) THEN
            NWCOEFF( IORD ) = NWCOEFF( IORD ) / 2 - 7
         END IF
      END DO
      MODIFY = .FALSE.
      GLOBAL_PLOT = .FALSE.

*  Loop until no more changes necessary (ie orders all consistent).
      MODIFIED = .TRUE.
      DO WHILE ( MODIFIED )

*     Initialise flag and statistics.
         MODIFIED = .FALSE.
         DO IORD = 1, N_ORDERS
            MEAN_DEVIATION( IORD ) = 0.0
            MEANSQ_DEVIATION( IORD ) = 0.0
            SAMPLE_COUNT( IORD ) = 0
         END DO

*        Loop thru each x increment using order polynomials to predict
*        y coords.
         x_step = MAX( 1, nx / ( maximum_sample / 4 )  )
         DO i = 1, nx, x_step
            count = 0

*           Loop thru orders evaluating order-specific polynomial
            DO iord = 1, n_orders

*            If a polynomial is NOT available then flag bad value
             IF ( polynomials( 1, iord ) .EQ. ECH__BAD_DOUBLE ) THEN
                fit_coord( iord ) = ECH__BAD_REAL

             ELSE

*              Evaluate order polynomial at x increment, and x+1 increment
               CALL ECH_FEVAL( ' ', maximum_poly,
     :              polynomials(1,iord),
     :              1, FLOAT( i ), fit_coord( iord ),
     :              status )
               CALL ECH_FEVAL( ' ', maximum_poly,
     :              polynomials(1,iord),
     :              1, FLOAT( i + 1 ), next,
     :              status )

*              If x and x+1 samples are in a stable polynomial region then
               IF ( ABS( fit_coord( iord ) - next ) .LT. 1.0 .AND.
     :              fit_coord( iord ) .GT. 0.0 .AND.
     :              fit_coord( iord ) .LT. FLOAT( ny )   ) THEN

*                 Save order number and y coordinate
                  count = count + 1
                  o_number( count ) = DBLE( iord )
                  y_coords( count ) = fit_coord( iord )
                  fit_work_xreal( count ) = 1.0
               ENDIF
             ENDIF
            END DO

*           Fit a polynomial thru the order-number/y-coordinates
            nrej = 0
            thresh = 10.0
            temp_poly = ipoly - 1
            CALL ECH_FITTER(
     :           'POLY',
     :           TEMP_POLY,
     :           TEMP_COEFFS,
     :           COUNT,
     :           O_NUMBER,
     :           Y_COORDS,
     :           FIT_WORK_XREAL,
     :           NREJ,
     :           THRESH,
     :           STATUS
     :          )

*           Loop evaluating predicted y coordinates for each order at this increment
            DO iord = 1, n_orders
               fit2_coord = temp_coeffs( ipoly )
               DO ii = temp_poly, 1, -1
                      fit2_coord = fit2_coord
     :                                 * DBLE( iord ) +
     :                              temp_coeffs( ii )
               END DO
               guess( iord ) = REAL( fit2_coord )

*              If a good coordinate was obtained then
               IF ( fit_coord(iord) .NE. ECH__BAD_REAL ) THEN

*                 Calculate deviation from one fit to the other and update stats
                  deviation = ABS ( fit_coord(iord) - guess(iord) )

*              Else use a fake (large) deviation
               ELSE
                  deviation = 2.0 * consistency_thresh
               ENDIF

*              Update statistics
               mean_deviation (iord )  = mean_deviation (iord ) +
     :                                           deviation
               meansq_deviation ( iord ) = meansq_deviation (iord) +
     :                             deviation * deviation
               sample_count ( iord )  = sample_count ( iord )  + 1

            END DO
         END DO

*        Calculate mean and standard deviation of differences
         DO iord = 1, n_orders
            mean_deviation ( iord )  = mean_deviation ( iord )  /
     :                         FLOAT ( sample_count ( iord )  )
            meansq_deviation ( iord ) = meansq_deviation ( iord ) /
     :                        FLOAT ( sample_count ( iord ) )
            sigma_deviation ( iord ) = SQRT ( ABS (
     :                                  meansq_deviation ( iord )  -
     :                           mean_deviation ( iord ) *
     :                           mean_deviation ( iord ) )  )
            dev_count ( iord ) = 0
         END DO

*        Process each order checking if each point deviates significantly
*        from a consistent fit
         xcount = 0

*        Loop thru x increments
         DO i = 1, nx, x_step
            xcount = xcount + 1
            count = 0
            x_trace_coord ( xcount ) = DBLE ( i )

*           Loop thru orders evaluating order specific polynomial
            DO iord = 1, n_orders

*            If a polynomial is NOT available then flag bad value
             IF ( polynomials ( 1, iord ) .EQ. ECH__BAD_DOUBLE ) THEN
               fit_coord(iord) = ECH__BAD_REAL

             ELSE

*              Evaluate order polynomial at x increment
               CALL ECH_FEVAL( ' ', maximum_poly,
     :              polynomials(1,iord),
     :              1, FLOAT( i ), fit_coord(iord),
     :              status )
               CALL ECH_FEVAL( ' ', maximum_poly,
     :              polynomials(1,iord),
     :              1, FLOAT( i + 1 ), next,
     :              status )

*              If x and x+1 samples are in a stable polynomial region then
               IF ( ABS ( fit_coord(iord) - next ) .LT. 1.0 .AND.
     :              fit_coord(iord) .GT. 0.0 .AND.
     :              fit_coord(iord) .LE. FLOAT (ny)   ) THEN

*                 Save order number and y coordinate
                  count = count + 1
                  o_number ( count ) = DBLE ( iord )
                  y_coords ( count ) = fit_coord(iord)
                  fit_work_xreal(count)=1.0
               ENDIF
             ENDIF
            END DO

*           Fit a polynomial thru the order-number/y-coordinates
            temp_poly = ipoly - 1
            CALL ECH_FITTER(
     :           'POLY',
     :           temp_poly,
     :           temp_coeffs,
     :           count,
     :           o_number,
     :           y_coords,
     :           fit_work_xreal,
     :           nrej,
     :           thresh,
     :           status
     :          )

*           Loop evaluating predicted y coordinates for each order at this increment
            DO iord = 1, n_orders
               fit2_coord  = temp_coeffs ( ipoly )
               DO ii = temp_poly, 1, -1
                      fit2_coord   = fit2_coord
     :                                 * DBLE ( iord ) +
     :                              temp_coeffs ( ii )
               END DO

*              Record prediction for order
               guess ( iord ) = REAL(fit2_coord)

*              If a valid point obtained from original order polynomial then
               IF ( fit_coord(iord) .NE. ECH__BAD_REAL ) THEN

*                 Calculate difference between the two fits at this increment
                  deviation = ABS ( fit_coord(iord) - guess(iord) )

*                 If point is TOO deviant then use new prediction
                  IF ( deviation .GT. consistency_thresh ) THEN
                     trace_re_fits ( xcount, iord ) = guess ( iord )
                     dev_count ( iord ) = dev_count ( iord ) + 1

*                 Else use original prediction
                  ELSE
                     trace_re_fits ( xcount, iord ) =
     :                                            fit_coord ( iord )
                  ENDIF

*              Else use new prediction
               ELSE
                  trace_re_fits ( xcount, iord ) = guess ( iord )
                  dev_count ( iord ) = dev_count ( iord ) + 1
               ENDIF
            END DO
         END DO

*        Report details of deviation between the two types of fit
         DO IORD = 1, N_ORDERS
            IF ( POLYNOMIALS( 1, IORD ) .NE. ECH__BAD_DOUBLE ) THEN
               IF ( DEV_COUNT( IORD ) .NE. 0 ) THEN
                  WRITE ( REPORT_STRING, 1003 ) IORD,
     :                  MEAN_DEVIATION( IORD ), SIGMA_DEVIATION( IORD )
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  WRITE ( REPORT_STRING, 1004 ) IORD, DEV_COUNT( IORD )
                  CALL ECH_REPORT( 0, REPORT_STRING )
               END IF

            ELSE
               WRITE ( REPORT_STRING, 1003 ) IORD,
     :               MEAN_DEVIATION( IORD ), SIGMA_DEVIATION( IORD )
               CALL ECH_REPORT( 0, REPORT_STRING )
               WRITE ( REPORT_STRING, 1006 ) IORD
               CALL ECH_REPORT( 0, REPORT_STRING )
            ENDIF
         END DO

*        Determine 'worst' order
         WORST = 1
         MAX_DEV = 0.0
         DO IORD = 1, N_ORDERS
            IF ( POLYNOMIALS( 1, IORD ) .NE. ECH__BAD_DOUBLE ) THEN
               IF ( SIGMA_DEVIATION( IORD ) *
     :              FLOAT( DEV_COUNT( IORD ) ) .GT. MAX_DEV ) THEN
                  MAX_DEV = SIGMA_DEVIATION( IORD ) *
     :                      FLOAT( DEV_COUNT( IORD ) )
                  WORST = IORD
               END IF
            END IF
         END DO

         selected = -1
         DO WHILE ( selected .NE. 0 )
          IF ( selected .GT. 0 ) THEN
             worst = selected

          ELSE IF ( selected .LT. 0 ) THEN
             selected = 0
          ENDIF

*         Loop reporting details for each order
          DO iord = 1, n_orders

*          If 'worst' order AND number of deviating points is significant then
           IF (  ( iord .EQ. worst ) .AND.
     :             (  ( worst .EQ. selected ) .OR. interactive .OR.
     :                  ( dev_count ( iord ) .GT.
     :               INT ( re_fit_thresh  * FLOAT ( xcount ) )))) THEN

*           Re-fit a polynomial for the order
            re_fit_count ( iord ) = re_fit_count ( iord ) + 1
            poly_degree = MIN ( re_fit_count (iord)/3 +3,
     :                          nwcoeff(iord),
     :                          maximum_poly )
            CALL ECH_FITTER(
     :           'POLY',
     :           poly_degree,
     :           temp_coeffs,
     :           xcount,
     :           x_trace_coord,
     :           trace_re_fits(1,iord),
     :           fit_work_xreal,
     :           nrej,
     :           thresh,
     :           status
     :          )

*           Evaluate new fit and plot it
            count = 0
            DO i = 1, nx, x_step
               count = count + 1
               CALL ECH_FEVAL( 'POLY', poly_degree,
     :              temp_coeffs,
     :              1, REAL(x_trace_coord(count)),
     :              y_graph(count),
     :              status )
            END DO
            DO i = 1, xcount
               x_graph( i ) = REAL( x_trace_coord( i ) )
               fit_work_xdouble( i ) = y_graph( i )
            END DO
            IF ( polynomials ( 1,iord ) .EQ. ECH__BAD_DOUBLE ) THEN
               WRITE ( title, 1006 ) iord

            ELSE
               WRITE ( title, 1005 ) iord,
     :               INT ( 100 * FLOAT ( dev_count ( iord ) ) /
     :                           FLOAT ( xcount )  )
            ENDIF

            IF ( .NOT. GLOBAL_PLOT ) THEN
             CALL ECH_GR_SET_COLOUR( COL_RED )
             OPTIONS = GRPH_CALC_MINMAX
             CALL ECH_PLOT_GRAPH(
     :            XCOUNT,
     :            X_GRAPH,
     :            Y_GRAPH,
     :            0.0, 0.0, 0.0, 2.0,
     :            'X pixels',
     :            'Y pixels',
     :            TITLE,
     :            0.0, 0.0, OPTIONS, '+',
     :            STATUS )

*            Re-Evaluate old fit and plot it over the top of the new one
             count = 0
             old_poly_degree = 0
             DO i = maximum_poly, 1, -1
               IF ( polynomials ( i,iord ) .NE. 0.0 .AND.
     :                  ( old_poly_degree .EQ. 0 )   )
     :                                       old_poly_degree = i
             END DO
             DO i = 1, nx, x_step
               count = count + 1
               CALL ECH_FEVAL(
     :              ' ', maximum_poly,
     :              polynomials(1,iord),
     :              1, REAL(x_trace_coord(count)),
     :              y_graph(count),
     :              status )
             END DO
             DO i = 1, xcount
               fit_work_xdouble  ( i ) = DBLE ( y_graph ( i ) )
             END DO
             CALL ECH_GR_SET_COLOUR( COL_BLACK )
             OPTIONS = GRPH_OVERLAY
             CALL ECH_PLOT_GRAPH( xcount, x_graph, y_graph,
     :            0.0, 0.0, 0.0, 0.0, ' ', ' ', ' ',
     :            0.0, 0.0, options, 'LINES', status )

            ELSE
             CALL ECH_PLOT_TRACES( nx, ny, IMAGE, n_orders,
     :            maximum_poly, polynomials, x_trace_coord,
     :            fit_work_xdouble, status )
             OPTIONS = GRPH_SET_COLOUR
             CALL ECH_GR_SET_COLOUR( COL_RED )
             OPTIONS = GRPH_OVERLAY
             CALL ECH_PLOT_GRAPH( xcount, x_graph, y_graph,
     :            0.0, 0.0, 0.0, 0.0, 'X pixels', 'Y pixels',
     :            title, 0.0, 0.0, options, '+', status )
            ENDIF

*           If we are in user interactive mode then
            IF ( interactive ) THEN

*              Decide if user wants to accept/reject the new polynomial fit
               IF (  dev_count ( worst ) .LE.
     :               INT ( re_fit_thresh  * FLOAT ( xcount ) ) ) THEN
                  CALL ECH_REPORT( 0, ' ' )
                  CALL ECH_REPORT( 0,
     :                  ' The orders are all consistent to within'//
     :                  ' the constraints specified by ' )
                  CALL ECH_REPORT( 0,
     :                  ' the tuning parameters TUNE_TRCNS and'//
     :                  ' TUNE_CNSDEV. The program has ' )
                  CALL ECH_REPORT( 0,
     :                  ' chosen the least consistent order.' )
               ENDIF
               CALL ECH_REPORT( 0, ' ' )
               CALL ECH_REPORT( 0, ' Options (default = Y):' )
               CALL ECH_REPORT( 0, ' ' )
               CALL ECH_REPORT( 0,
     :                  '   E         Exit saving current polynomials' )
               CALL ECH_REPORT( 0,
     :                  '   Y         Accept new polynomial for order' )
               CALL ECH_REPORT( 0,
     :                  '   D         Disable order from overall fit' )
               CALL ECH_REPORT( 0,
     :                  '   S         Select order to be re-fit' )
               CALL ECH_REPORT( 0,
     :                  '   G         Global plot - all orders shown' )
               CALL ECH_REPORT( 0,
     :             '   -         Decrement degree of inter-order fit' )
               CALL ECH_REPORT( 0,
     :             '   +         Increment degree of inter-order fit' )
               CALL ECH_REPORT( 0, ' ' )
               CALL ECH_REPORT( 0, ' ' )
               CALL ECH_READ_GRPH_CURSOR ( status )
               modify = .FALSE.
               global_plot = .FALSE.

*              If user wants to exit, set no order selected
               IF ( user_input_char .EQ. 'E' ) THEN
                  WRITE ( report_string, 1002 ) iord
                  CALL ECH_REPORT( 0, report_string )
                  selected = 0

*              Else if they choose to abandon this order, then ensure it
*                   is flagged as inactive
               ELSE IF ( user_input_char .EQ. 'D' ) THEN
                  DO ii = 1, maximum_poly
                     polynomials( ii, iord ) = ECH__BAD_DOUBLE
                  END DO
                  WRITE ( report_string, 1001 ) iord
                  CALL ECH_REPORT( 0, report_string )
                  modified = .TRUE.
                  selected = 0

*              Else if they want to see the re-fit for a specific order
               ELSE IF ( user_input_char .EQ. 'S' ) THEN
                  value = FLOAT( worst )
                  CALL ECH_GET_PARAMETER(
     :                 'INSTANT-PROMPT=Order to re-fit',
     :                 'INT', value, .FALSE., ' ', 0,
     :                 status )
                  IF ( SELECTED .LE. 0 ) THEN
                     SELECTED = -1
                     CALL ECH_REPORT( 0, ' No order selected.' )

                  ELSE
                     selected = MIN( n_orders,
     :                       MAX( 1, INT( value ) ) )
                     WRITE ( report_string, 1009 ) selected
                     CALL ECH_REPORT( 0, report_string )
                  END IF

*              Else if they want to see all orders plotted together.
               ELSE IF ( user_input_char .EQ. 'G' ) THEN
                  global_plot = .TRUE.
                  modified = .TRUE.

*              Else if - then alter degree of inter-order fit
               ELSE IF ( user_input_char .EQ. '-' ) THEN
                  IF ( ipoly .GE. 3 ) THEN
                     ipoly = ipoly - 1
                     WRITE ( report_string, 1007 ) ipoly-1
                  ELSE
                     report_string =
     :                   'Cannot decrement degree of inter-order fit'
                  ENDIF
                  CALL ECH_REPORT( 0, report_string )
                  modified = .TRUE.
                  selected = 0

*              Else if + then alter degree of inter-order fit
               ELSE IF ( user_input_char .EQ. '+' ) THEN
                  IF ( ipoly .LE. maximum_poly .AND.
     :                 ipoly .LE. n_orders ) THEN
                     ipoly = ipoly + 1
                     WRITE ( report_string, 1008 ) ipoly-1
                  ELSE
                     report_string =
     :                   'Cannot increment degree of inter-order fit'
                  ENDIF
                  CALL ECH_REPORT( 0, report_string )
                  modified = .TRUE.
                  selected = 0

*              Else (default) we keep the previous fit
               ELSE
                  modify = .TRUE.
                  selected = 0
               ENDIF

*           Else set modification required flag by default
            ELSE
               IF ( re_fit_count( iord ) .LT. n_orders )
     :            modify = .TRUE.
            ENDIF

*           If order polynomial is to be replaced by the new one, then
            IF ( modify ) THEN
               modified = .TRUE.

*              Zero all coefficients
               DO II = 1, MAXIMUM_POLY
                  POLYNOMIALS( II, IORD ) = 0.0
               END DO

*              Copy new coefficients
               DO II = 1, POLY_DEGREE
                  POLYNOMIALS( II, IORD ) = TEMP_COEFFS( II )
               END DO
               WRITE ( report_string, 1000 ) iord
               CALL ECH_REPORT( 0, report_string )
            ENDIF
           ENDIF
          END DO
         END DO
      END DO

*  Reset plot colour to black.
      CALL ECH_GR_SET_COLOUR( COL_BLACK )

  999 CONTINUE

 1000 FORMAT ( 1X, 'Order ', I3, ': new polynomial accepted.' )
 1001 FORMAT ( 1X, 'Order ',I3, ': to be predicted from other orders.' )
 1002 FORMAT ( 1X, 'Order ', I3, ': exit: saving current polynomials.' )
 1003 FORMAT ( 1X, 'Order ', I3, ': mean diff= ', 1PE8.2,
     :                           ' sigma diff= ', E8.2, '.' )
 1004 FORMAT ( 1X, 'Order ', I3, ': ', I4, ' deviant points.' )
 1005 FORMAT ( 1X, 'Old/New(+) fits for Order ', I3,
     :         ': % bad points = ', I3 )
 1006 FORMAT ( 1X, 'Order ', I3, ': predicted trace from other orders.')
 1007 FORMAT ( 1X, 'Degree of inter-order fit decremented to ', I3, '.')
 1008 FORMAT ( 1X, 'Degree of inter-order fit incremented to ', I3, '.')
 1009 FORMAT ( 1X, 'Order ', I3,': predicting trace ', I3, '.' )

      END
