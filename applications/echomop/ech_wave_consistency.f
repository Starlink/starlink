      SUBROUTINE ECH_WAVE_CONSISTENCY(
     :           NX,
     :           MAX_FEATURES,
     :           FTR_LIST,
     :           N_ORDERS,
     :           AUTOMATIC,
     :           CONSISTENCY_THRESH,
     :           RE_FIT_THRESH,
     :           SEAR_START_WAVE,
     :           SEAR_END_WAVE,
     :           MAXIMUM_SAMPLE,
     :           MAXIMUM_POLY,
     :           N_POLY,
     :           WAVE_COEFFS,
     :           MAX_PERM_FTRS,
     :           INPUT_FTR_POSITIONS,
     :           OBS_STRENGTH,
     :           ORDER_IDNUMS,
     :           IDENTIFIED_FTRS,
     :           IDEN_FTR_POSITION,
     :           IDEN_FTR_STATUS,
     :           IDEN_FTR_WAVELENGTH,
     :           X_TRACE_COORD,
     :           TRACE_RE_FITS,
     :           FIT_WORK_XREAL,
     :           FIT_WORK_XDOUBLE,
     :           FIT_WORK_3XDOUBLE,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_WAVE_CONSISTENCY

*  Purpose:
*     Check consistency of all order wavelengths.

*  Invocation:
*     CALL ECH_WAVE_CONSISTENCY(
*     :    NX,
*     :    MAX_FEATURES,
*     :    FTR_LIST,
*     :    N_ORDERS,
*     :    AUTOMATIC,
*     :    CONSISTENCY_THRESH,
*     :    RE_FIT_THRESH,
*     :    SEAR_START_WAVE,
*     :    SEAR_END_WAVE,
*     :    MAXIMUM_SAMPLE,
*     :    MAXIMUM_POLY,
*     :    N_POLY,
*     :    WAVE_COEFFS,
*     :    MAX_PERM_FTRS,
*     :    INPUT_FTR_POSITIONS,
*     :    OBS_STRENGTH,
*     :    ORDER_IDNUMS,
*     :    IDENTIFIED_FTRS,
*     :    IDEN_FTR_POSITION,
*     :    IDEN_FTR_STATUS,
*     :    IDEN_FTR_WAVELENGTH,
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
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     CONSISTENCY_THRESH = REAL (Given)
*        Max permitted deviation on pixels between fits.
*     RE_FIT_THRESH = REAL (Given)
*        Maximum fraction of deviant points permitted before re-fit.
*     SEAR_START_WAVE = REAL (Given)
*        Minimum wavelengths per order.
*     SEAR_END_WAVE = REAL (Given)
*        Maximum wavelengths per order.
*     AUTOMATIC = LOGICAL (Given)
*        TRUE if automatic mode.
*     MAXIMUM_SAMPLE = INTEGER (Given)
*        Maximum number of samples to be used in fit.
*     MAX_PERM_FTRS = INTEGER (Given)
*        Maximum number of features to use in a fit.
*     INPUT_FTR_POSITIONS = REAL (Given)
*        Observed central positions of features.
*     OBS_STRENGTH = REAL (Given)
*        Observed strengths (usually intensity) of features.
*     ORDER_IDNUMS = INTEGER (Given)
*        ID number of each order.
*     WAVE_COEFFS = DOUBLE (Given and Returned)
*        Array of polynomial coefficients for each order.
*     IDENTIFIED_FTRS = INTEGER (Given and Returned)
*        Count of identified features.
*     IDEN_FTR_POSITION = REAL (Given and Returned)
*        Positions identified features.
*     IDEN_FTR_STATUS = INTEGER (Given and Returned)
*        Statuses of identified features.
*     IDEN_FTR_WAVELENGTH = REAL (Given and Returned)
*        Wavelengths of identified features.
*     FIT_WAVES = REAL (Temporary Workspace)
*        Work array for polynoimal fitting.
*     FIT_WAVES2 = REAL (Temporary Workspace)
*        Work array for polynoimal fitting.
*     FIT_WAVES_WORK = REAL (Temporary Workspace)
*        Work array for polynoimal fitting.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Workspace for array of x-pixel coords.
*     TRACE_RE_FITS = DOUBLE (Temporary Workspace)
*        Workspace for array to-be-re-fitted y pixel coords.
*     FIT_WORK_XREAL = REAL (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     FIT_WORK_XDOUBLE = DOUBLE (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     FIT_WORK_3XDOUBLE = DOUBLE (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*     FTR_LIST = REAL (Given)
*        List of known arc line wavelengths.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     N_POLY = INTEGER (Given)
*        Maximum degree of polynomial to attempt to fit.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If less than 3 orders give up now
*      Loop until no more changes necessary (ie orders all consistent)
*        Loop through each x increment using order polynomials to predict
*        wavelengths
*           Loop through orders evaluating order specific polynomial
*            If a polynomial is NOT available then flag bad value
*            Else
*              Evaluate wavelength polynomial at x increment, and x+1 increment
*              If x and x+1 samples are in a stable polynomial region then
*                 Save order number and wavelength
*              Endif
*            Endif
*           End loop
*           Fit a polynomial through the order-number/wavelengths
*           Loop evaluating predicted y coordinates for each order at this increment
*              If a good coordinate was obtained then
*                 Calculate deviation from one fit to the other and update stats
*              Else use a fake (large) deviation
*              Endif
*              Update statistics
*           End loop
*        End loop
*        Calculate mean and standard deviation of differences
*        Loop through x increments
*           Loop through orders evaluating order specific polynomial
*            If a polynomial is NOT available then flag bad value
*            Else
*              Evaluate order polynomial at x increment
*              If x and x+1 samples are in a stable polynomial region then
*                 Save order number and y coordinate
*              Endif
*            Endif
*           End loop
*           Fit a polynomial through the order-number/y-coordinates
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
*        Loop reporting details for each order
*          If 'worst' order AND number of deviating points is significant then
*           Re-fit a polynomial for the order
*           Evaluate new fit and plot it
*           Re-Evaluate old fit and plot it over the top of the new one
*         Predict wavelength for feature position
*         Search for nearest matching wavelength in database
*         If match is close enough then
*             Flag feature as identified, use feature strength to as weight
*         Endif
*       End loop
*       Fit a polynomial through the feature wavelength/position data
*           If we are in user interactive mode then
*              Decide if user wants to accept/reject the new polynomial fit
*              If user accepts fit, set a modify flag
*              Else if they choose to abandon this order, then ensure it
*                   is flagged as inactive
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
      INCLUDE 'ECH_FEATURE.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER MAX_FEATURES
      REAL FTR_LIST( MAX_FEATURES )
      INTEGER N_POLY
      INTEGER MAXIMUM_POLY
      INTEGER MAXIMUM_SAMPLE
      INTEGER N_ORDERS
      REAL CONSISTENCY_THRESH
      REAL RE_FIT_THRESH
      DOUBLE PRECISION WAVE_COEFFS( MAXIMUM_POLY, N_ORDERS )
      LOGICAL AUTOMATIC
      INTEGER MAX_PERM_FTRS
      INTEGER ORDER_IDNUMS( N_ORDERS )
      REAL INPUT_FTR_POSITIONS( MAX_PERM_FTRS, N_ORDERS )
*          ! Observed central positions of features.
      REAL OBS_STRENGTH( MAX_PERM_FTRS, N_ORDERS )
*          ! Observed strengths (usually intensity) of features.
      REAL SEAR_START_WAVE( N_ORDERS )
*          ! Minimum wavelengths per order.
      REAL SEAR_END_WAVE( N_ORDERS )
*          ! Maximum wavelengths per order.
      INTEGER IDENTIFIED_FTRS( N_ORDERS )

*  Arguments Returned:
      REAL IDEN_FTR_POSITION( MAX_PERM_FTRS, N_ORDERS )
*          ! Positions identified features.
      REAL IDEN_FTR_WAVELENGTH( MAX_PERM_FTRS, N_ORDERS )
*          ! Positions identified features.
      INTEGER IDEN_FTR_STATUS( MAX_PERM_FTRS, N_ORDERS )
*          ! Positions identified features.

*  Workspace:
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION TRACE_RE_FITS( NX, N_ORDERS )
      REAL FIT_WORK_XREAL( NX )
      DOUBLE PRECISION FIT_WORK_XDOUBLE( NX )
      DOUBLE PRECISION FIT_WORK_3XDOUBLE( 3 * NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION O_NUMBER( MAX_ALLOWED_ORDERS )
*          ! Order numbers.
      DOUBLE PRECISION Y_COORDS( MAX_ALLOWED_ORDERS )
*          ! Y-coordinates of order centres.
      DOUBLE PRECISION TEMP_COEFFS( MAX_ID_FTRS / 2 )
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
      REAL CHANS( MAX_FIT_FTRS )
      REAL WAVES( MAX_FIT_FTRS )
      REAL WWEIGHTS( MAX_FIT_FTRS )
      REAL RMSES( MAX_FIT_FTRS )
      REAL FITS( MAX_FIT_FTRS )
      REAL X_GRAPH( 2500 )
      REAL Y_GRAPH( 2500 )
      REAL MEANDEV_AT_X( 500 )
      REAL RDUMMY( 2 )
      REAL RMS
      REAL THRESH
      REAL WORST
      REAL MAX_DEV
      REAL NEXT
      REAL PREDICT
      REAL WAVE_FIT_DELTA
      REAL POSSIBLE
      REAL DEVIATION

      INTEGER sample_count( max_allowed_orders )
*          ! Count of contributors to sigma estimate.
      INTEGER dev_count( max_allowed_orders )
*          ! Count of 'too' deviant points.
      INTEGER re_fit_count( max_allowed_orders )
*          ! Count of re-fits per order.
      INTEGER NWCOEFF( MAX_ALLOWED_ORDERS )
      INTEGER STATI( MAX_FIT_FTRS )
      INTEGER NLID
      INTEGER LOOPS
      INTEGER NEWCOUNT
      INTEGER NREJ
      INTEGER X_STEP
      INTEGER XCOUNT
      INTEGER POLY_DEGREE
      INTEGER OLD_POLY_DEGREE
      INTEGER I
      INTEGER II
      INTEGER III
      INTEGER IORD
      INTEGER OPTIONS
      INTEGER POSCOUNT
      INTEGER NO_OF_POSITIONS
      INTEGER COUNT
      INTEGER IPOLY
      INTEGER TEMP_POLY

      LOGICAL DONE( 256 )
      LOGICAL ONEDONE
      LOGICAL INTERACTIVE
      LOGICAL CHECK
      LOGICAL MODIFY
      LOGICAL MODIFIED

      CHARACTER*64 TITLE

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      REAL ARC_ARFIND
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  If less than 3 orders give up now.
      INTERACTIVE = .NOT. AUTOMATIC
      IF ( n_orders .LT. 3 ) THEN
         GO TO 999
      END IF

      LOOPS = 0
      IPOLY = 3
      DO IORD = 1, N_ORDERS
         RE_FIT_COUNT( IORD ) = 0
         DONE( IORD ) = .FALSE.
         DO I = 1, MAXIMUM_POLY
            IF ( WAVE_COEFFS( I, IORD ) .NE. 0.0 ) NWCOEFF( IORD ) = I
         END DO
      END DO

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

*     Loop through each X-increment using order polynomials to predict
*     wavelengths.
         X_STEP = MAX( 1, NX / ( MAXIMUM_SAMPLE / 4 ) )
         XCOUNT = 0
         DO I = 1, NX, X_STEP
            COUNT = 0
            XCOUNT = XCOUNT + 1
            MEANDEV_AT_X( XCOUNT ) = 0.0

*        Loop through orders evaluating order-specific polynomial.
            DO IORD = 1, N_ORDERS

*           If a polynomial is NOT available then flag bad value.
               IF ( WAVE_COEFFS( 1, IORD ) .EQ. ECH__BAD_DOUBLE .OR.
     :              WAVE_COEFFS( 1, IORD ) .EQ. 0.0 ) THEN
                  FIT_COORD( IORD ) = ECH__BAD_REAL

               ELSE

*              Evaluate wavelength polynomial at X increment, and X+1 increment.
                  RDUMMY( 1 ) = FLOAT( I )
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :                 WAVE_COEFFS( 1, IORD ),
     :                 1, RDUMMY, FIT_COORD( IORD ), STATUS )
                  RDUMMY( 1 ) = FLOAT( I + 1 )
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :                 WAVE_COEFFS( 1, IORD ),
     :                 1, RDUMMY, NEXT, STATUS )

*              If X and X+1 samples are in a stable polynomial region then.
                  IF ( ABS( FIT_COORD( IORD ) - NEXT ) .LT. 1.0 .AND.
     :                 FIT_COORD( IORD ) .GE.
     :                 SEAR_START_WAVE( IORD ) .AND.
     :                 FIT_COORD( IORD ) .LE.
     :                 SEAR_END_WAVE( IORD ) ) THEN

*                 Save order number and wavelength.
                     COUNT = COUNT + 1
                     O_NUMBER( COUNT ) = DBLE( ORDER_IDNUMS( IORD ) )
                     Y_COORDS( COUNT ) = FIT_COORD( IORD )
                     FIT_WORK_XREAL( COUNT ) = 1.0
                  END IF
               END IF
            END DO

*        Fit a polynomial through the order-number/wavelengths.
            NREJ = 0
            THRESH = 10.0
            TEMP_POLY = IPOLY - 1
            DO III = 1, MAX_ID_FTRS / 2
               TEMP_COEFFS( III ) = 0.0
            END DO
            CALL ECH_FITTER( 'POLY', TEMP_POLY, TEMP_COEFFS,
     :           COUNT, O_NUMBER, Y_COORDS, FIT_WORK_XREAL,
     :           NREJ, THRESH, STATUS )

*        Loop evaluating predicted Y-coordinates for each order at this
*        increment.
            DO IORD = 1, N_ORDERS
               FIT2_COORD  = TEMP_COEFFS( IPOLY )
               DO II = TEMP_POLY, 1, -1
                  FIT2_COORD = FIT2_COORD *
     :                  DBLE( ORDER_IDNUMS( IORD ) ) + TEMP_COEFFS( II )
               END DO
               GUESS( IORD ) = REAL( FIT2_COORD )

*           If a good coordinate was obtained then
               IF ( FIT_COORD( IORD ) .NE. ECH__BAD_REAL ) THEN

*              Calculate deviation from one fit to the other and update stats
                  DEVIATION = ABS( FIT_COORD( IORD ) - GUESS( IORD ) )

*           Else use a fake (large) deviation.
               ELSE
                  DEVIATION = 10.0
               END IF

*            Update statistics.
               IF ( IDENTIFIED_FTRS( IORD ) .GT. 0 ) THEN
                  IF ( ( I .LT. INT( IDEN_FTR_POSITION( 1, IORD ) ) .OR.
     :                   I .GT. INT( IDEN_FTR_POSITION
     :                   ( IDENTIFIED_FTRS( IORD ), IORD ) ) ) ) THEN
                     MEAN_DEVIATION( IORD ) = MEAN_DEVIATION( IORD ) +
     :                     DEVIATION
                     MEANSQ_DEVIATION( IORD ) =
     :                     MEANSQ_DEVIATION( IORD ) +
     :                     DEVIATION * DEVIATION
                     SAMPLE_COUNT( IORD ) = SAMPLE_COUNT( IORD )  + 1
                     MEANDEV_AT_X( XCOUNT ) = MEANDEV_AT_X( XCOUNT ) +
     :                     DEVIATION
                  END IF
               END IF
            END DO
         END DO

*     Calculate mean and standard deviation of differences.
         DO IORD = 1, N_ORDERS
            MEAN_DEVIATION( IORD ) = MEAN_DEVIATION( IORD ) /
     :            FLOAT( SAMPLE_COUNT( IORD ) + 1 )
            MEANSQ_DEVIATION( IORD ) = MEANSQ_DEVIATION( IORD ) /
     :            FLOAT( SAMPLE_COUNT( IORD ) + 1 )
            SIGMA_DEVIATION( IORD ) = SQRT( ABS(
     :            MEANSQ_DEVIATION( IORD )  -
     :            MEAN_DEVIATION( IORD ) *
     :            MEAN_DEVIATION( IORD ) ) )
            DEV_COUNT( IORD ) = 0
         END DO
         DO I = 1, XCOUNT
            MEANDEV_AT_X( I ) = MEANDEV_AT_X( I ) / FLOAT( N_ORDERS )
         END DO

*     Process each order checking if each point deviates significantly
*     from a consistent fit.
         XCOUNT = 0

*     Loop through X-increments.
         DO I = 1, NX, X_STEP
            XCOUNT = XCOUNT + 1
            COUNT = 0
            X_TRACE_COORD( XCOUNT ) = DBLE( I )

*        Loop through orders evaluating order specific polynomial.
            DO IORD = 1, N_ORDERS

*           If a polynomial is NOT available then flag bad value.
               IF ( WAVE_COEFFS( 1, IORD ) .EQ. ECH__BAD_DOUBLE .OR.
     :              WAVE_COEFFS( 1, IORD ) .EQ. 0.0 ) THEN
                  FIT_COORD( IORD ) = ECH__BAD_REAL

               ELSE

*              Evaluate order polynomial at x increment.
                  RDUMMY( 1 ) = FLOAT( I )
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :                 WAVE_COEFFS( 1, IORD ),
     :                 1, RDUMMY, FIT_COORD( IORD ), STATUS )
                  RDUMMY( 1 ) = FLOAT( I + 1 )
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :                 WAVE_COEFFS( 1, IORD ), 1, RDUMMY, NEXT,
     :                 STATUS )

*              If x and x+1 samples are in a stable polynomial region then
*              save order number and Y-coordinate.
                  IF ( ABS( FIT_COORD( IORD ) - NEXT ) .LT. 1.0 .AND.
     :                 FIT_COORD( IORD ) .GE.
     :                 SEAR_START_WAVE( IORD ) .AND.
     :                 FIT_COORD( IORD ) .LE.
     :                 SEAR_END_WAVE( IORD ) ) THEN
                     COUNT = COUNT + 1
                     O_NUMBER( COUNT ) = DBLE( ORDER_IDNUMS( IORD ) )
                     Y_COORDS( COUNT ) = FIT_COORD( IORD )
                     FIT_WORK_XREAL( COUNT ) = 1.0
                  END IF
               END IF
            END DO

*        Fit a polynomial through the order-number/Y-coordinates.
            TEMP_POLY = IPOLY - 1
            DO III = 1, MAX_ID_FTRS / 2
               TEMP_COEFFS( III ) = 0.0
            END DO

*!---Problem in type of fit_work_xreal in this call?? MJC 25-APR-1996.
            CALL ECH_FITTER(
     :           'POLY', TEMP_POLY, TEMP_COEFFS, COUNT,
     :           O_NUMBER, Y_COORDS, FIT_WORK_XREAL,
     :           NREJ, THRESH, STATUS )

*        Loop evaluating predicted Y-coordinates for each order at this
*        increment.
            DO iord = 1, n_orders
               fit2_coord  = temp_coeffs ( ipoly )
               DO ii = temp_poly, 1, -1
                      fit2_coord   = fit2_coord
     :                         * DBLE ( order_idnums ( iord ) ) +
     :                              temp_coeffs ( ii )
               END DO

*           Record prediction for order.
               guess( iord ) = REAL( fit2_coord )

*           If a valid point obtained from original order polynomial then.
               IF ( fit_coord( iord ) .NE. ECH__BAD_REAL ) THEN

*              Calculate difference between the two fits at this increment.
                  deviation = ABS( fit_coord( iord ) - guess( iord ) )

*              If point is TOO deviant then use new prediction.
                  IF ( identified_ftrs (iord) .EQ. 0 ) THEN
                     trace_re_fits ( xcount, iord ) = guess ( iord )
                     dev_count ( iord ) = dev_count ( iord ) + 1

                  ELSE IF ( ( i .LT.
     :                          INT ( iden_ftr_position(1,iord) ) .OR.
     :                   i .GT. INT ( iden_ftr_position
     :                         (identified_ftrs(iord),iord) ) )  .AND.
     :                    deviation .GT. consistency_thresh ) THEN
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
         IF ( diagnostics_active ) THEN
          DO iord = 1, n_orders
            WRITE ( report_string, 1003 ) iord,mean_deviation(iord),
     :                                     sigma_deviation(iord)
            CALL ECH_REPORT( 0, report_string )
            IF ( wave_coeffs ( 1,iord ) .NE. ECH__BAD_DOUBLE .AND.
     :            wave_coeffs ( 1, iord ) .NE. 0.0 ) THEN
               WRITE ( report_string, 1004 ) iord, dev_count ( iord )
            ELSE
               WRITE ( report_string, 1006 ) iord
            ENDIF
            CALL ECH_REPORT( 0, report_string )
          END DO
         ENDIF

*        Determine 'worst' order
         worst = 0.0
         max_dev = 0.0
         DO iord = 1, n_orders
           IF ( .NOT. done ( iord ) ) THEN
            IF ( wave_coeffs ( 1, iord ) .NE. ECH__BAD_DOUBLE .AND.
     :            wave_coeffs ( 1, iord ) .NE. 0.0 ) THEN
               IF ( mean_deviation ( iord ) *
     :                  FLOAT ( dev_count (iord)+1 )
     :                                            .GT. max_dev ) THEN
                  max_dev = mean_deviation ( iord ) *
     :                          FLOAT ( dev_count (iord)+1 )
                  worst = iord
               ENDIF
            ENDIF
           ENDIF
         END DO

*     Loop reporting details for each order.
         ONEDONE = .FALSE.
         DO IORD = 1, N_ORDERS

*        If 'worst' order AND number of deviating points is significant.
            IF ( IORD .EQ. WORST ) THEN
               DONE( IORD ) = .TRUE.
               ONEDONE = .TRUE.

*           Re-fit a polynomial for the order
               re_fit_count( iord ) = re_fit_count( iord ) + 1
               poly_degree = n_poly
               DO iii = 1, max_id_ftrs / 2
                  temp_coeffs( iii ) = 0.0
               END DO
               CALL ECH_FITTER( 'POLY', POLY_DEGREE, TEMP_COEFFS,
     :              COUNT, X_TRACE_COORD, TRACE_RE_FITS( 1, IORD ),
     :              FIT_WORK_XREAL, NREJ, THRESH, STATUS )

*           Evaluate new fit and plot it.
               COUNT = 0
               DO I = 1, NX, X_STEP
                  COUNT = COUNT + 1
                CALL ECH_FEVAL( 'POLY', POLY_DEGREE, TEMP_COEFFS,
     :               1, REAL( X_TRACE_COORD( COUNT ) ),
     :               SNGL( TRACE_RE_FITS( COUNT, IORD ) ), STATUS )
               END DO
               DO I = 1, XCOUNT
                  X_GRAPH( I ) = REAL( X_TRACE_COORD( I ) )
                  Y_GRAPH( I ) = REAL( TRACE_RE_FITS( I, IORD ) )
               END DO
               WAVE_FIT_DELTA = 3 * ABS( Y_GRAPH( XCOUNT ) -
     :               Y_GRAPH( 1 ) ) / X_GRAPH( XCOUNT )
               IF ( WAVE_COEFFS( 1, IORD ) .EQ. ECH__BAD_DOUBLE .OR.
     :              WAVE_COEFFS( 1, IORD ) .EQ. 0.0 ) THEN
                  WRITE ( TITLE, 1006 ) IORD

               ELSE
                  WRITE ( TITLE, 1005 ) IORD,
     :                  INT( 100 * FLOAT( DEV_COUNT( IORD ) ) /
     :                  FLOAT( XCOUNT ) )
               END IF
            IF ( diagnostics_active ) THEN
             options = grph_calc_minmax
             CALL ECH_PLOT_GRAPH( xcount, x_graph, y_graph,
     :            0., 0., 0., 0., 'X pixels', 'Y pixels', title,
     :            0.0, 0.0, options, '+', status )

*           Re-Evaluate old fit and plot it over the top of the new one
             count = 0
             old_poly_degree = 0
             DO i = maximum_poly, 1, -1
                IF ( wave_coeffs( i,iord ) .NE. 0.0 .AND.
     :               old_poly_degree .EQ. 0 )
     :             old_poly_degree = i
             END DO
             DO i = 1, nx, x_step
                  count = count + 1
                rdummy( 1 ) = float( i )
                CALL ECH_FEVAL( ' ', old_poly_degree,
     :               wave_coeffs( 1, iord ), 1, rdummy,
     :               SNGL( trace_re_fits( count, iord ) ), status )
             END DO
             DO i = 1, xcount
                y_graph( i ) = trace_re_fits( i, iord )
             END DO
             options = grph_overlay
             CALL ECH_PLOT_GRAPH( xcount, x_graph, y_graph,
     :            0., 0., 0., 0., ' ', ' ', ' ',
     :            0.0, 0.0, options, 'LINES', status )
            ENDIF

        no_of_positions = 0
        DO WHILE ( input_ftr_positions(no_of_positions+1,iord)
     :                                                 .GT. 0.0 )
           no_of_positions = no_of_positions + 1
        END DO

        poscount = 0
        nlid = 0
        newcount = 0
        IF ( loops .GT. 4 .OR. identified_ftrs(iord) .EQ. 0 ) THEN
         DO i = 1, no_of_positions


          check = .FALSE.
          IF ( identified_ftrs(iord) .EQ. 0 ) THEN
             check = .TRUE.
          ELSE IF ( input_ftr_positions(i,iord) .LT.
     :              iden_ftr_position(1,iord)
     :                   .OR.
     :         input_ftr_positions(i,iord) .GT.
     :         iden_ftr_position(identified_ftrs(iord),iord) ) THEN
             check = .TRUE.
          ENDIF

          IF ( check ) THEN

*         Predict wavelength for feature position
            PREDICT = REAL( TEMP_COEFFS( POLY_DEGREE ) )
            DO II = POLY_DEGREE - 1, 1, -1
               PREDICT = PREDICT * INPUT_FTR_POSITIONS( I, IORD ) +
     :               REAL( TEMP_COEFFS( II ) )
            END DO

*         Search for nearest matching wavelength in database
          possible = ARC_ARFIND ( ftr_list, max_features,
     :                                       predict )

*         If match is close enough then
          IF ( ABS ( possible-predict ) .LE.
     :                       wave_fit_delta ) THEN

*             Flag feature as identified, use feature strength to as weight
             nlid = nlid + 1
             newcount = newcount + 1
             chans ( nlid ) = input_ftr_positions ( i,iord )
             waves ( nlid ) = possible
             stati ( nlid ) = ftr_auto_ident + ftr_cnsis_added

             WRITE ( report_string, 1009 ) predict,possible,
     :                                      possible-predict
             CALL ECH_REPORT( 0, report_string )
          ENDIF

         ELSE IF ( poscount .EQ. 0 .AND.
     :              input_ftr_positions(i,iord) .GE.
     :             iden_ftr_position(1,iord) ) THEN

            DO poscount = 1, identified_ftrs(iord)
               nlid = nlid + 1
               chans ( nlid ) = iden_ftr_position ( poscount,iord )
               waves ( nlid ) = iden_ftr_wavelength ( poscount,iord )
               stati ( nlid ) = iden_ftr_status ( poscount,iord )
            END DO
         END IF
         END DO
        ENDIF

*       Fit a polynomial through the feature wavelength/position data
       IF ( newcount .GT. 0 .AND. loops .GT. 4) THEN
        DO ii = 1, max_fit_ftrs
           wweights ( ii ) = 1.0
        END DO

        DO iii = 1, max_id_ftrs / 2
           temp_coeffs ( iii ) = 0.0
        END DO
        CALL ECH_WAVE_POLYFIT ( chans, waves, stati,wweights,
     :                          rmses, fits, .TRUE.,
     :                          nlid, nx,
     :                          nwcoeff(iord), .TRUE.,
     :                          nwcoeff(iord),
     :                          temp_coeffs, rms )

        IF ( rms .LT. wave_fit_delta / 3.0 ) THEN
            CALL ECH_REPORT( 0, ' ! Using new lines in future fits.' )
            DO ii= 1, nlid
              iden_ftr_position ( ii,iord ) = chans ( ii )
              iden_ftr_wavelength ( ii,iord ) = waves ( ii )
              iden_ftr_status ( ii,iord ) = stati ( ii )
            END DO
            identified_ftrs ( iord ) = nlid
        ENDIF
       ENDIF

*           If we are in user interactive mode then
            IF ( interactive .AND. diagnostics_active ) THEN

*              Decide if user wants to accept/reject the new polynomial fit
               CALL ECH_REPORT( 0, ' ' )
               CALL ECH_REPORT( 0, 'Options (default = Y):' )
               CALL ECH_REPORT( 0, ' ' )
               CALL ECH_REPORT( 0,
     :              '   E         Exit saving polynomials ' )
               CALL ECH_REPORT( 0,
     :              '   Y         Accept new polynomial ' )
               CALL ECH_REPORT( 0,
     :              '   D         Disable order from overall fit' )
               CALL ECH_REPORT( 0,
     :             '   -         Decrement degree of inter-order fit' )
               CALL ECH_REPORT( 0,
     :             '   +         Increment degree of inter-order fit' )
               CALL ECH_REPORT( 0, ' ' )
               CALL ECH_READ_GRPH_CURSOR ( status )
               modify = .FALSE.

*              If user accepts fit, set a modify flag
               IF ( user_input_char .EQ. 'E' ) THEN
                  WRITE ( report_string, 1002 ) iord
                  CALL ECH_REPORT( 0, report_string )

*              Else if they choose to abandon this order, then ensure it
*                   is flagged as inactive
               ELSE IF ( user_input_char .EQ. 'D' ) THEN
                  DO ii = 1, maximum_poly
                     wave_coeffs ( ii, iord ) = ECH__BAD_DOUBLE
                  END DO
                  WRITE ( report_string, 1001 ) iord
                  CALL ECH_REPORT( 0, report_string )
                  modified = .TRUE.

*              Else if - then alter degree of inter-order fit
               ELSE IF ( user_input_char .EQ. '-' ) THEN
                  ipoly = MAX ( 1, ipoly - 1 )
                  WRITE ( report_string, 1007 ) ipoly
                  CALL ECH_REPORT( 0, report_string )
                  modified = .TRUE.

*              Else if + then alter degree of inter-order fit
               ELSE IF ( user_input_char .EQ. '+' ) THEN
                  ipoly = MIN ( maximum_poly, ipoly + 1 )
                  WRITE ( report_string, 1008 ) ipoly
                  CALL ECH_REPORT( 0, report_string )
                  modified = .TRUE.

*              Else (default) we keep the previous fit
               ELSE
                  modify = .TRUE.
               ENDIF

*           Else set modification required flag by default
            ELSE
               IF ( RE_FIT_COUNT( IORD ) .LT. N_ORDERS )
     :            MODIFY = .TRUE.
            END IF

*           If order polynomial is to be replaced by the new one, then
            IF ( modify ) THEN
               modified = .TRUE.

*              Zero all coefficients
               DO ii = 1, maximum_poly
                  wave_coeffs ( ii, iord ) = 0.0
               END DO

*              Copy new coefficients
               DO ii = 1, poly_degree
                  wave_coeffs ( ii, iord ) = temp_coeffs ( ii )
               END DO
               WRITE ( report_string, 1000 ) iord
               CALL ECH_REPORT( 0, report_string )
            ENDIF
           ENDIF
         END DO
         IF ( .NOT. ONEDONE ) THEN
            DO II = 1, N_ORDERS
               DONE( II ) = .FALSE.
            END DO
            LOOPS = LOOPS + 1
            MODIFIED = .TRUE.
            IF ( LOOPS .GT. 6 ) MODIFIED = .FALSE.
         END IF
      END DO

  999 CONTINUE

 1000 FORMAT ( 1X, 'Order ',I3,': New polynomial accepted.' )
 1001 FORMAT ( 1X, 'Order ',I3,': To be predicted from other orders.')
 1002 FORMAT ( 1X, 'Order ',I3,': Exit - saving current polynomials.')
 1003 FORMAT ( 1X, 'Order ',I3,': Mean diff= ', E8.2,
     :         '  Sigma diff= ', E8.2, '.' )
 1004 FORMAT ( 1X, 'Order ',I3,': Count of deviant points= ', I4, '.' )
 1005 FORMAT ( 1X, 'Old/New(+) fits for Order ', I3,
     :         ': % bad points=', I3 )
 1006 FORMAT ( 1X, 'Order ',I3,': Predicted waves from other orders.')
 1007 FORMAT ( 1X, 'Degree of inter-order fit decremented to ', I3 )
 1008 FORMAT ( 1X, 'Degree of inter-order fit incremented to ', I3 )
 1009 FORMAT ( 1X, 'Possible new feature ', F16.4, F16.4, F16.4 )

      END
