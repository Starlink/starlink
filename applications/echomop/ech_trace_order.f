      SUBROUTINE ECH_TRACE_ORDER(
     :           IMAGE,
     :           QUALITY,
     :           NX,
     :           NY,
     :           N_ORDERS,
     :           ORDER_CENTRE,
     :           TRACE_WIDTH,
     :           ORDER_NUMBER,
     :           ORDER_SLOPE,
     :           TRACE_MODE,
     :           XBOX,
     :           MAXIMUM_SAMPLE,
     :           MAX_BAD_SAMPLES,
     :           USE_MEDIAN,
     :           MAXIMUM_POLY,
     :           AFITTER,
     :           N_POLY,
     :           TRACE,
     :           INITIAL_DEV,
     :           POLYNOMIAL,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_TRACE_ORDER

*  Purpose:
*     Trace order path across frame.

*  Description:
*     This routine attempts to trace the path of an order. Starting at
*     the central column it steps out to the left/right and determines
*     local centroids. When sufficient points have been determined a
*     polynomial fit is made to the dataset.

*  Invocation:
*     CALL ECH_TRACE_ORDER(
*     :    IMAGE,
*     :    QUALITY,
*     :    NX,
*     :    NY,
*     :    N_ORDERS,
*     :    ORDER_CENTRE,
*     :    TRACE_WIDTH,
*     :    ORDER_NUMBER,
*     :    ORDER_SLOPE,
*     :    TRACE_MODE,
*     :    XBOX,
*     :    MAXIMUM_SAMPLE,
*     :    MAX_BAD_SAMPLES,
*     :    USE_MEDIAN,
*     :    MAXIMUM_POLY,
*     :    AFITTER,
*     :    N_POLY,
*     :    TRACE,
*     :    INITIAL_DEV,
*     :    POLYNOMIAL,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    STATUS
*     :   )

*  Arguments:
*    IMAGE = REAL( NX, NY ) (Given)
*       Input frame image of dimensions nx columns and ny rows.
*    QUALITY = BYTE( NX, NY ) (Given)
*       Input frame image quality.
*    NX = INTEGER (Given)
*       Number of columns in frame.
*    NY = INTEGER (Given)
*       Number of rows in frame.
*    N_ORDERS = INTEGER (Given)
*       Number of orders in echellogram.
*    ORDER_CENTRE = INTEGER (Given)
*       Pixel row where order centre is located.
*    TRACE_WIDTH = INTEGER (Given)
*       Estimated trace width.
*    ORDER_NUMBER = INTEGER (Given)
*       Number of order being processed.
*    ORDER_SLOPE = REAL (Given)
*       Estimate of the slope of the orders.
*    TRACE_MODE = CHAR (Given)
*       Tracing mode selected may be:
*
*           B - c.o.g. balance
*           E - edge interpolation
*           G - Gaussian fit to profile
*           C - centroid profile
*           T + ? - Triangle filtered
*           A + ? - Auto mode switching
*           U + ? - User-supplied estimated trace coords
*           R + ? - Retrace using input poly as baseline
*
*    XBOX = INTEGER (Given)
*       Size of x sampling box for mean/median.
*    MAXIMUM_SAMPLE = INTEGER (Given)
*       Maximum number of sampling boxes to use in x direction.
*    MAX_BAD_SAMPLES = INTEGER (Given)
*       Maximum number of consequtive bad samples.
*    USE_MEDIAN = LOGICAL (Given)
*       TRUE to select median filtering, else mean.
*    MAXIMUM_POLY = INTEGER (Given)
*       Maximum degree of polynomial to attempt to fit.
*    N_POLY = INTEGER (Given)
*       Maximum degree of polynomial to attempt to fit.
*    TRACE = REAL (Returned)
*       Array of nx estimates of order centre in each column.
*    INITIAL_DEV = REAL (Returned)
*       Array of initial deviations from polynomial fit.
*    POLYNOMIAL = DOUBLE (Returned)
*       Array of Polynomial coefficients (constant first).
*    X_TRACE_COORD = DOUBLE (Temporary workspace)
*       Workspace array for x points to be fitted.
*    Y_TRACE_COORD = DOUBLE (Temporary workspace)
*       Workspace array for y points to be fitted.
*    FIT_WORK_XINT = INTEGER (Temporary workspace)
*       Workspace array for NAG routine in fitter.
*    FIT_WORK_XDOUBLE = DOUBLE (Temporary workspace)
*       Workspace array for NAG routine in fitter.
*    FIT_WORK_3XDOUBLE = DOUBLE (Temporary workspace)
*       Workspace array for NAG routine in fitter.
*    STATUS = INTEGER (Given and Returned)
*       Input/Output status conditions.
*    AFITTER = CHAR (Given)
*       Type of fitting function to use.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     12-MAR-96 (MJC):
*       Standard Prologue.  Array bounds checking.
*     18-APR-1996 (MJC):
*       Fixed plotting bug, modified plot titles.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_GRAPHICS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      BYTE QUALITY( NX, NY )
      INTEGER N_ORDERS
      REAL ORDER_SLOPE
      INTEGER ORDER_NUMBER    ! Lowest numbered order on the image frame.
      INTEGER ORDER_CENTRE
      INTEGER IUEMAX_AT
      REAL IUE_MAX
      CHARACTER*( * ) TRACE_MODE
      INTEGER MAXIMUM_POLY
      INTEGER N_POLY
      LOGICAL USE_MEDIAN
      INTEGER XBOX            ! Sampling box for mean/median.
      INTEGER MAXIMUM_SAMPLE  ! Maximum number of x sample boxes to use.
      INTEGER MAX_BAD_SAMPLES ! Bad samples allowed.
      CHARACTER*( * ) AFITTER

*  Arguments Returned:
      INTEGER TRACE_WIDTH
      REAL TRACE( NX )
      REAL INITIAL_DEV( NX )
      DOUBLE PRECISION POLYNOMIAL( MAXIMUM_POLY )

*  Workspace:
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER TFN_LIM
      PARAMETER ( TFN_LIM = 100 )

      INTEGER SAM_LIM
      PARAMETER ( SAM_LIM = 512 )

      INTEGER MAX_GOOD_SAMPLE
      PARAMETER ( MAX_GOOD_SAMPLE = 5000 )

*  Local Variables:
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )

      REAL GOODSAMPLE( MAX_GOOD_SAMPLE )
      REAL TRIANGLE_FN( - TFN_LIM : TFN_LIM )
      REAL XVS( MAX_CURS_POSITIONS )
      REAL YVS( MAX_CURS_POSITIONS )
      REAL SAMPLE( SAM_LIM )
      REAL FSAMPLE( SAM_LIM )
      REAL CENTRE
      REAL EST_TRACE
      REAL XOFF
      REAL Y_TRACE
      REAL VMIN
      REAL VMAX
      REAL VMIN2
      REAL VMAX2

      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER IGOOD
      INTEGER CENTRE_ROW
      INTEGER I_STEP
      INTEGER X_STEP
      INTEGER NUM_COLS
      INTEGER NUM_ROWS
      INTEGER MIN_COLS
      INTEGER MIN_ROWS
      INTEGER MAX_COLS
      INTEGER MAX_ROWS
      INTEGER INC_COLS
      INTEGER INC_ROWS
      INTEGER X_LOW
      INTEGER Y_LOW
      INTEGER Y_HI
      INTEGER BAD_LEFT
      INTEGER BAD_RIGHT
      INTEGER COUNT
      INTEGER OPTIONS
      INTEGER N_SAMPLE
      INTEGER YINDEX
      INTEGER LEFT_EXTENT
      INTEGER RIGHT_EXTENT
      INTEGER POINTS_TO_FIT
      INTEGER ISAMP
      INTEGER ITRI
      INTEGER LOPLOT
      INTEGER HIPLOT
      INTEGER LEFT_SUCCESS
      INTEGER RIGHT_SUCCESS
      INTEGER SUCCESS_RATE
      INTEGER GSTATUS
      INTEGER LAST_ORDER
      INTEGER N_GOOD
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3
      INTEGER NCHAR4

      LOGICAL LEFT_OK
      LOGICAL RIGHT_OK
      LOGICAL GOT_A_CENTRE
      LOGICAL BAD_SAMPLE
      LOGICAL EDGE
      LOGICAL GAUSSIAN
      LOGICAL BALANCE
      LOGICAL USER_COORDS
      LOGICAL RE_TRACE
      LOGICAL TRIANGLE
      LOGICAL AUTO_SWITCH

      CHARACTER*80 TPLOT_TITLE
      CHARACTER*80 INPUT_MODE
      CHARACTER*80 FITTER
      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2
      CHARACTER*8 REF_STR3
      CHARACTER*8 REF_STR4
      CHARACTER*1 MODE_SELECT
      CHARACTER*1 MODE_BACKUP

      COMMON / IM_LIMITS_R / VMIN, VMAX
      COMMON / IM_LIMITS_I / LAST_ORDER

*  Functions called:
      EXTERNAL ECH_FATAL_ERROR
      LOGICAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR ( STATUS ) ) RETURN

*  Setup mode variables initial states.
      AUTO_SWITCH = .FALSE.
      TRIANGLE = .FALSE.
      GAUSSIAN = .FALSE.
      EDGE = .FALSE.
      BALANCE = .FALSE.
      USER_COORDS = .FALSE.
      RE_TRACE = .FALSE.
      BAD_SAMPLE = .FALSE.
      FITTER = AFITTER( :8 )
      IF ( AFITTER .EQ. 'IUE' ) THEN
         FITTER = 'POLY'
         CALL ECH_REPORT( 0, ' IUE-specific tracing enabled.' )
      END IF

*  If 'U' mode (user-supplied coords) selected, use next character.
      INPUT_MODE = TRACE_MODE
      MODE_SELECT = TRACE_MODE( :1 )
      IF ( mode_select .EQ. 'U' .OR. mode_select .EQ. 'u') THEN
         user_coords = .TRUE.
         CALL ECH_REPORT( 0, ' Re-tracing user-supplied coordinates.' )
         trace_mode = trace_mode( 2: )
         points_to_fit = 0
         CALL ECH_GET_PARAMETER( 'XPIXELS', 'ARRAY', xvs,
     :        .FALSE., 'NPIXELS', points_to_fit, status )
         points_to_fit = 0
         CALL ECH_GET_PARAMETER( 'YPIXELS', 'ARRAY', yvs,
     :        .FALSE., 'NPIXELS', points_to_fit, status )
         IF ( STATUS .EQ. 0 ) THEN
            IF ( points_to_fit .LE. 0 ) THEN
             CALL ECH_REPORT( 0,
     :            ' Zero or negative number of pixels selected.' )
             STATUS=-1
            END IF
         END IF
         IF ( STATUS .NE. 0 ) THEN
          CALL ECH_REPORT( 0,
     :    ' Unable to get X-Y position(s) for user trace-estimate.' )
          CALL ECH_REPORT(0,
     :    ' These are held in the user variables:' )
          CALL ECH_REPORT(0,
     :    ' NPIXELS - number of selected pixels,' )
          CALL ECH_REPORT(0,
     :    ' XPIXELS and YPIXELS - the pixel positions.' )
          CALL ECH_REPORT(0,
     :    ' These are usually set using the space bar in task ICUR.' )
          GO TO 999

         ELSE
            DO I = 1, POINTS_TO_FIT
               X_TRACE_COORD( I ) = DBLE( XVS( I ) )
               Y_TRACE_COORD( I ) = DBLE( YVS( I ) )
               INITIAL_DEV( I ) = 0.5
            END DO

*        Fit a polynomial.
            CALL ECH_FITTER( FITTER, N_POLY, TEMP_COEFFS,
     :           POINTS_TO_FIT, X_TRACE_COORD, Y_TRACE_COORD,
     :           INITIAL_DEV, 0, 5.0, STATUS )

         END IF
      END IF

*  If 'R' mode (re-tracing using old poly)  selected, use next character
      mode_select = trace_mode( :1 )
      IF ( MODE_SELECT .EQ. 'R' .OR. MODE_SELECT .EQ. 'r' ) THEN
         CALL ECH_REPORT( 0,' Re-tracing using current trace as base.' )
         RE_TRACE = .TRUE.
         TRACE_MODE = TRACE_MODE( 2: )

*     Use old coefficients
         DO I = 1, N_POLY
            TEMP_COEFFS( I ) = POLYNOMIAL( I )
         END DO
      END IF

*  If 'A' mode (automatic switching) selected, use next character
      mode_select = trace_mode( :1)
      IF ( mode_select .EQ. 'A' .OR. mode_select .EQ. 'a' ) THEN
         CALL ECH_REPORT( 0,' Tracing with automatic mode switching.' )
         auto_switch = .TRUE.
         mode_select = trace_mode( 2:2 )

*     Check for 'T' mode also set
         IF ( mode_select .EQ. 'T' .OR. mode_select .EQ. 't' ) THEN
            CALL ECH_REPORT( 0, ' Triangle filtering enabled.' )
            triangle = .TRUE.
            mode_select = trace_mode( 3:3 )
         END IF

*  Else if 'T' mode (triangle filtering) selected, use next character
      ELSE IF ( mode_select .EQ. 'T' .OR. mode_select .EQ. 't' ) THEN
         CALL ECH_REPORT( 0,' Triangle filtering enabled.' )
         triangle = .TRUE.
         mode_select = trace_mode( 2:2 )

*     Check for 'A' mode also set
         IF ( mode_select .EQ. 'A' .OR. mode_select .EQ. 'a' ) THEN
            CALL ECH_REPORT( 0,
     :           ' Tracing with automatic mode-switching.' )
            auto_switch = .TRUE.
            mode_select = trace_mode( 3:3 )
         END IF
      END IF

*  Determine mode G, E, B or C (default) flags.
      IF ( mode_select .EQ. 'G' .OR. mode_select .EQ. 'g' ) THEN
         GAUSSIAN = .TRUE.

      ELSE IF ( mode_select .EQ. 'E' .OR. mode_select .EQ. 'e' ) THEN
         EDGE = .TRUE.

      ELSE IF ( mode_select .EQ. 'B' .OR. mode_select .EQ. 'b' ) THEN
         BALANCE = .TRUE.
      END IF

*  Initialise arrays.
      DO I = 1, MAXIMUM_POLY
         POLYNOMIAL( I ) = 0.0
      END DO
      CALL ECH_ZERO_DBLE( NX, X_TRACE_COORD( 1 ) )
      CALL ECH_ZERO_DBLE( NX, Y_TRACE_COORD( 1 ) )
      POINTS_TO_FIT = 0

*  Determine sampling box range and start size, and increment to increase
*  by in the case of failure.
      MAX_ROWS = MIN( 100, 3 * NY / N_ORDERS / 2 )
      MIN_ROWS = MAX( 8, TRACE_WIDTH )
      IF ( EDGE .OR. BALANCE ) THEN
         MIN_ROWS = TRACE_WIDTH + 4
      END IF
      IF ( N_ORDERS .EQ. 1 ) THEN
         MIN_ROWS = MAX( 8, 2 * TRACE_WIDTH )
      END IF
      MIN_COLS = 1
      MAX_COLS = MIN( 100, 10 * TRACE_WIDTH )
      IF ( AFITTER .EQ. 'IUE' ) THEN
         MIN_COLS = XBOX
         MAX_COLS = XBOX
         MIN_ROWS = 7
         MAX_ROWS = 15
      END IF
      INC_ROWS = ( MAX_ROWS - MIN_ROWS ) / 5 + 1
      INC_COLS = ( MAX_COLS - MIN_COLS ) / 5 + 1
      LEFT_OK = .TRUE.
      RIGHT_OK = .TRUE.

      IF  ( AFITTER .EQ. 'IUE' ) THEN
         DO yindex = -3, +3
            N_GOOD = 0
            DO I = 0, 50
               IF ( QUALITY( I + NX / 2 - 25, ORDER_CENTRE + YINDEX )
     :              .EQ. 0 .AND.
     :              IMAGE( I + NX / 2 - 25, ORDER_CENTRE + YINDEX ) .NE.
     :              ECH__BAD_REAL ) THEN
                   N_GOOD = N_GOOD + 1
                   GOODSAMPLE( N_GOOD ) = IMAGE( I + NX / 2 - 25,
     :                   ORDER_CENTRE + YINDEX )
               END IF
            END DO
            CALL ECH_MEAN_MEDIAN( N_GOOD, GOODSAMPLE,
     :           .TRUE., .FALSE., SAMPLE( YINDEX + 4 ), STATUS )
         END DO
         IUEMAX_AT = 4
         IUE_MAX = -1.0E20
         DO I = 1, 7
            IF ( SAMPLE( I ) .GT. IUE_MAX ) THEN
               IUEMAX_AT = I
               IUE_MAX = SAMPLE( I )
            END IF
         END DO
         IF ( IUEMAX_AT .NE. 4 ) THEN
           CALL CHR_ITOC( ORDER_CENTRE, REF_STR1, NCHAR1 )
           CALL CHR_ITOC( ORDER_CENTRE - 4 + IUEMAX_AT, REF_STR2,
     :          NCHAR2 )
           REPORT_STRING = ' Mean order position (' //
     :           REF_STR1( :NCHAR1 ) // ') corrected to ' //
     :           REF_STR2( :NCHAR2 ) // '.'
           CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
         ORDER_CENTRE = ORDER_CENTRE - 4 + IUEMAX_AT
      END IF

*  Setup x increment per sample, and initial y coord of trace at
*  frame centre.
      X_STEP = MAX( 1, NX / MAXIMUM_SAMPLE )
      IF ( AFITTER .EQ. 'IUE' ) X_STEP = 1
      LEFT_EXTENT = NX
      RIGHT_EXTENT = 1
      LEFT_SUCCESS = 0
      RIGHT_SUCCESS = 0

*  Loop progressively stepping out (-x_step) pixels further from the
*  centre column.
      Y_TRACE = FLOAT( ORDER_CENTRE )

      DO i_step = 1, nx / 2, x_step
         num_rows = min_rows
         num_cols = min_cols

*     If still tracing left hand extent.
         IF ( left_ok ) THEN
            got_a_centre = .FALSE.

*        Loop trying to determine centre.
            DO WHILE ( .NOT. ( got_a_centre .OR. bad_sample ) )

*           Calulate starting limits of sampling box
               X_LOW = MAX( 1, NX/2 - I_STEP - NUM_COLS / 2 )
               Y_LOW = INT( Y_TRACE + 0.5 ) - NUM_ROWS / 2
               Y_HI = INT( Y_TRACE + 0.5 ) + NUM_ROWS / 2
               CENTRE_ROW = INT( Y_TRACE + 0.5 )

*           Revise row limits in the case of triangle filtering.
               IF ( TRIANGLE ) THEN
                  y_low = y_low - num_rows/2
                  y_hi = y_hi + num_rows/2
                  DO itri = -num_rows/2, num_rows/2
                     triangle_fn ( itri ) = 0.75 *
     :                           ( 1.0 - ABS ( FLOAT ( itri ) /
     :                                 FLOAT ( num_rows/2 ) )  )
     :                               + 0.25
                  END DO
               END IF

*           Calculate mean/median over sample row/columns.
               n_sample = 0
               DO yindex = y_low, y_hi
                 n_sample = n_sample + 1
                 IF ( yindex .GT. 0 .AND. yindex .LE. ny .AND.
     :                x_low+num_cols .LE. nx ) THEN
                    N_GOOD = 0
                    DO IGOOD = 0, NUM_COLS - 1
                       IF ( QUALITY( IGOOD + X_LOW, YINDEX ) .EQ. 0
     :                      .AND. IMAGE( IGOOD + X_LOW, YINDEX ) .NE.
     :                      ECH__BAD_REAL ) THEN
                          N_GOOD = N_GOOD + 1
                          GOODSAMPLE( N_GOOD ) = IMAGE( IGOOD + X_LOW,
     :                                                  YINDEX )
                       END IF
                    END DO
                    IF ( N_GOOD .GT. 1 ) THEN
                        CALL ECH_MEAN_MEDIAN( N_GOOD, GOODSAMPLE,
     :                       USE_MEDIAN, .FALSE., SAMPLE( N_SAMPLE ),
     :                       STATUS )

                    ELSE
                       SAMPLE( N_SAMPLE ) = GOODSAMPLE( 1 )
                    END IF

                 ELSE IF ( 2*centre_row-yindex .GT. 0 .AND.
     :                     2*centre_row-yindex .LE. ny .AND.
     :                         x_low+num_cols .LE. nx ) THEN
                    N_GOOD = 0
                    DO IGOOD = 0, NUM_COLS - 1
                       IF ( QUALITY( IGOOD + X_LOW, 2 * CENTRE_ROW -
     :                               YINDEX ) .EQ. 0 .AND.
     :                      IMAGE( IGOOD + X_LOW, 2 * CENTRE_ROW -
     :                             YINDEX ) .NE.
     :                      ECH__BAD_REAL ) THEN
                          N_GOOD = N_GOOD + 1
                          GOODSAMPLE( N_GOOD ) = IMAGE( IGOOD + X_LOW,
     :                          2 * CENTRE_ROW - YINDEX )
                       END IF
                    END DO
                    IF ( N_GOOD .GT. 1 ) THEN
                        CALL ECH_MEAN_MEDIAN( N_GOOD, GOODSAMPLE,
     :                       USE_MEDIAN, .FALSE., SAMPLE( N_SAMPLE ),
     :                       STATUS )

                    ELSE
                       SAMPLE( N_SAMPLE ) = GOODSAMPLE( 1 )
                    END IF

                 ELSE
                    sample( n_sample ) = 0.0
                 END IF
               END DO

*           If triangle filtering is enabled then apply it now.
               IF ( triangle ) THEN
                  DO isamp = num_rows/2 + 1, n_sample - num_rows/2
                    fsample( isamp ) = 0.0
                    DO itri = -num_rows / 2, num_rows / 2
                        fsample( isamp ) = fsample( isamp ) +
     :                                     sample( isamp + itri ) *
     :                                     triangle_fn( itri )
                    END DO
                  END DO
                  DO isamp = num_rows / 2 + 1, n_sample - num_rows / 2
                     sample( isamp - num_rows / 2 ) = fsample( isamp )
                  END DO
                  n_sample = n_sample - ( num_rows / 2 ) * 2

               END IF

*           Find centre of sampled mean/median intensity profile.
               CENTRE = FLOAT( NUM_ROWS / 2 + 1 )
               CALL ECH_FIND_CENTRE( MODE_SELECT, N_SAMPLE, SAMPLE,
     :              CENTRE, STATUS )

*           If we failed to get a reliable centre value.
               IF ( status .EQ. ECH__NO_CENTRE ) THEN

*              If automatic mode switching is enabled then
                  IF ( auto_switch ) THEN

*                 Select next least stringent mode and try again
                     centre = FLOAT( num_rows / 2 + 1 )
                     mode_backup = 'B'
                     IF ( gaussian ) mode_backup = 'C'
                     CALL ECH_FIND_CENTRE( MODE_BACKUP, N_SAMPLE,
     :                    SAMPLE, CENTRE, STATUS )
                  END IF
               END IF

*           If still no center found then increase sampling box
               IF ( STATUS .EQ. ECH__NO_CENTRE ) THEN
                  NUM_COLS = NUM_COLS + INC_COLS
                  NUM_ROWS = NUM_ROWS + INC_ROWS

*              If sampling box now too large, set bad sample flag
                  IF ( NUM_COLS .GT. MAX_COLS ) BAD_SAMPLE = .TRUE.
                  IF ( NUM_ROWS .GT. MAX_ROWS ) BAD_SAMPLE = .TRUE.

*           Else set centre found flag
               ELSE
                  got_a_centre = .TRUE.
               END IF
            END DO

*        Check that this centre is near enough to the last one found
            centre = centre - FLOAT( num_rows / 2 + 1 )
            IF ( ABS ( INT ( centre + .5 ) ) .GT. 3 )
     :                  got_a_centre = .FALSE.

            IF ( user_coords .OR. re_trace ) THEN
               CALL ECH_FEVAL( fitter, n_poly, temp_coeffs, 1,
     :              FLOAT( i_step ), est_trace, status )
               IF ( ABS( INT(
     :              FLOAT(centre_row)+centre-est_trace ) ) .GT. 3 )
     :            got_a_centre = .FALSE.
            END IF

*        If a good centre was obtained then
            IF ( got_a_centre ) THEN

*           Set current centre to nearest pixel
               IF ( INT( FLOAT(centre_row)+centre ) .GT. 0 .AND.
     :              INT( FLOAT(centre_row)+centre ) .LE. ny ) THEN
                  y_trace = FLOAT( centre_row ) + centre
               END IF
               bad_left = 0
               IF ( nx/2 - i_step .LT. left_extent )
     :                                   left_extent = nx/2 - i_step
               IF ( nx/2 - i_step .GT. right_extent )
     :                                   right_extent = nx/2 - i_step
               left_success = left_success + 1

*           Add coords to arrays of fittable coordinates
               points_to_fit = points_to_fit + 1
               x_trace_coord( points_to_fit ) =
     :               FLOAT( nx/2 - i_step )
               y_trace_coord( points_to_fit ) = y_trace
               IF ( afitter .EQ. 'IUE' )
     :            y_trace = FLOAT( order_centre ) -
     :                  order_slope * FLOAT( i_step )

*        Else if a bad sample then
            ELSE

*           increment bad-sample count and terminate
*           leftwards scan if too many bad-samples (>max_bad_samples consecutive)
               bad_left = bad_left + 1
               IF ( bad_left .GT. max_bad_samples ) THEN
                 IF ( order_number .NE. 1 .AND.
     :                order_number .NE. n_orders ) left_ok = .FALSE.
               END IF
               IF ( IAND( report_mode, rpm_full + rpm_error )
     :              .GT. 0 ) THEN
                  CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
                  CALL CHR_ITOC( X_STEP, REF_STR2, NCHAR2 )
                  REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :                  ' lost trace at X= ' //
     :                  REF_STR2( :NCHAR2 ) // '.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  CALL ECH_SET_CONTEXT( 'PROBLEM',' Lost left trace' )
               END IF

*           Estimate new centre of trace using old position and slope
               y_trace = y_trace - order_slope * FLOAT ( x_step )

*           If user-supplied coords have supplied an estimated trace or re-tracingthen
               IF ( user_coords .OR. re_trace ) THEN
                  y_trace = est_trace
                  left_ok = .TRUE.
               END IF
            END IF
            bad_sample = .FALSE.
         END IF
      END DO

*  Loop progressively stepping out (+x_step) pixels further from the centre column
      y_trace = FLOAT ( order_centre )

      DO i_step = 0, nx / 2, x_step
         num_rows = min_rows
         num_cols = min_cols

*     If still tracing right hand extent then
         IF ( right_ok ) THEN
            got_a_centre = .FALSE.

*        Loop trying to determine centre
            DO WHILE ( .NOT. ( got_a_centre .OR. bad_sample ) )

*           Calulate starting limits of sampling box
               x_low = MAX( 1, nx/2 + i_step - num_cols / 2 )
               y_low = INT( y_trace + 0.5 ) - num_rows / 2
               y_hi = INT( y_trace + 0.5 ) + num_rows / 2
               centre_row = INT( y_trace + 0.5 )

*           Revise row limits in the case of triangle filtering
               IF ( triangle ) THEN
                  y_low = y_low - num_rows / 2
                  y_hi = y_hi + num_rows / 2
                  DO itri = -num_rows / 2, num_rows / 2
                     triangle_fn( itri ) = 0.75 *
     :                     ( 1.0 - ABS( FLOAT( itri ) /
     :                     FLOAT( num_rows / 2 ) ) ) + 0.25
                  END DO
               END IF

*           Calculate mean/median over sample row/columns
               n_sample = 0
               DO yindex = y_low, y_hi
                 n_sample = n_sample + 1
                 IF ( yindex .GT. 0 .AND. yindex .LE. ny .AND.
     :                x_low+num_cols .LE. nx  ) THEN
                    N_GOOD = 0
                    DO IGOOD = 0, NUM_COLS - 1
                       IF ( QUALITY( IGOOD + X_LOW, YINDEX ) .EQ. 0
     :                      .AND. IMAGE( IGOOD + X_LOW, YINDEX ) .NE.
     :                      ECH__BAD_REAL ) THEN
                          N_GOOD = N_GOOD + 1
                          GOODSAMPLE( N_GOOD ) = IMAGE( IGOOD + X_LOW,
     :                                                  YINDEX )
                       END IF
                    END DO
                    IF ( N_GOOD .GT. 1 ) THEN
                       CALL ECH_MEAN_MEDIAN( N_GOOD, GOODSAMPLE,
     :                      USE_MEDIAN, .FALSE., SAMPLE( N_SAMPLE ),
     :                      STATUS )

                    ELSE
                       SAMPLE( N_SAMPLE ) = GOODSAMPLE( 1 )
                    END IF

                 ELSE IF ( 2*centre_row-yindex .GT. 0 .AND.
     :                     2*centre_row-yindex .LE. ny .AND.
     :                     x_low+num_cols .LE. nx  ) THEN
                    N_GOOD = 0
                    DO IGOOD = 0, NUM_COLS - 1
                       IF ( QUALITY( IGOOD + X_LOW, 2 * CENTRE_ROW -
     :                               YINDEX ) .EQ. 0 .AND.
     :                      IMAGE( IGOOD + X_LOW, 2 * CENTRE_ROW -
     :                             YINDEX ) .NE.
     :                      ECH__BAD_REAL ) THEN
                          N_GOOD = N_GOOD + 1
                          GOODSAMPLE( N_GOOD ) = IMAGE( IGOOD + X_LOW,
     :                          2 * CENTRE_ROW - YINDEX )
                       END IF
                    END DO
                    IF ( N_GOOD .GT. 1 ) THEN
                       CALL ECH_MEAN_MEDIAN( N_GOOD, GOODSAMPLE,
     :                      USE_MEDIAN, .FALSE., SAMPLE( N_SAMPLE ),
     :                      STATUS )

                    ELSE
                       SAMPLE( N_SAMPLE ) = GOODSAMPLE( 1 )
                    END IF

                 ELSE
                   sample( n_sample ) = 0.0
                 END IF
               END DO

*           If triangle filtering is enabled then apply it now
               IF ( triangle ) THEN
                  DO isamp = num_rows / 2 + 1, n_sample - num_rows / 2
                    fsample ( isamp ) = 0.0
                    DO itri = -num_rows / 2, num_rows / 2
                        fsample( isamp ) = fsample( isamp ) +
     :                                     sample( isamp + itri ) *
     :                                     triangle_fn( itri )
                    END DO
                  END DO
                  DO isamp = num_rows/2 + 1, n_sample - num_rows/2
                     sample( isamp - num_rows / 2 ) = fsample( isamp )
                  END DO
                  n_sample = n_sample - ( num_rows / 2 ) * 2
               END IF

*           Find centre of sampled mean/median intensity profile
               CENTRE = FLOAT( NUM_ROWS / 2 + 1 )
               CALL ECH_FIND_CENTRE( MODE_SELECT, N_SAMPLE, SAMPLE,
     :              CENTRE, STATUS )

*           If we failed to get a reliable centre value then
               IF ( status .EQ. ECH__NO_CENTRE ) THEN

*              If automatic mode switching is enabled then
                  IF ( auto_switch ) THEN

*                 Select next least stringent mode and try again
                     centre = FLOAT ( num_rows / 2 + 1 )
                     mode_backup = 'B'
                     IF ( gaussian ) mode_backup = 'C'
                     CALL ECH_FIND_CENTRE( MODE_BACKUP, N_SAMPLE,
     :                    SAMPLE, CENTRE, STATUS )
                  END IF
               END IF

*           If still no center found then increase sampling box
               IF ( status .EQ. ECH__NO_CENTRE ) THEN
                  num_cols = num_cols + inc_cols
                  num_rows = num_rows + inc_rows

*              If sampling box now too large, set bad sample flag
                  IF ( num_cols .GT. max_cols ) bad_sample = .TRUE.
                  IF ( num_rows .GT. max_rows ) bad_sample = .TRUE.

*           Else set centre found flag
               ELSE
                  got_a_centre = .TRUE.
               END IF
            END DO

*        Check that this centre is near enough to the last one found
            centre = centre - FLOAT( num_rows / 2 + 1 )
            IF ( ABS( INT( centre + .5 ) ) .GT. 3 )
     :         got_a_centre = .FALSE.

            IF ( user_coords .OR. re_trace ) THEN
               CALL ECH_FEVAL( fitter, n_poly, temp_coeffs,
     :              1, FLOAT( i_step ), est_trace, status )
               IF ( ABS( INT(
     :              FLOAT(centre_row)+centre-est_trace ) ) .GT. 3 )
     :            got_a_centre = .FALSE.
            END IF

*        If a good centre was obtained then
            IF ( got_a_centre ) THEN

*           Set current centre to nearest pixel
               IF ( INT ( FLOAT(centre_row)+centre ) .GT. 0 .AND.
     :              INT ( FLOAT(centre_row)+centre ) .LE. ny ) THEN
                  y_trace = FLOAT(centre_row) + centre
               END IF
               bad_right = 0
               IF ( nx / 2 + i_step .LT. left_extent )
     :            left_extent = nx / 2 + i_step
               IF ( nx / 2 + i_step .GT. right_extent )
     :            right_extent = nx / 2 + i_step
               right_success = right_success + 1

*           Add coords to arrays of fittable coordinates
               points_to_fit = points_to_fit + 1
               x_trace_coord( points_to_fit ) =
     :            FLOAT( nx / 2 + i_step )
               y_trace_coord( points_to_fit ) = y_trace
               IF ( afitter .EQ. 'IUE' )
     :            y_trace = FLOAT( order_centre ) +
     :                  order_slope * FLOAT( i_step )

*        Else if a bad sample then
            ELSE

*           increment bad-sample count and terminate
*           rightwards scan if too many bad-samples (>max_bad_samples consecutive)
               bad_right = bad_right + 1
               IF ( bad_right .GT. max_bad_samples ) THEN
                 IF ( order_number .NE. 1 .AND.
     :                order_number .NE. n_orders ) right_ok = .FALSE.
               END IF
               IF ( IAND( report_mode, rpm_full + rpm_error )
     :              .GT. 0 ) THEN
                  CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
                  CALL CHR_ITOC( X_STEP, REF_STR2, NCHAR2 )
                  REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :                  ' lost trace at X= ' //
     :                  REF_STR2( :NCHAR2 ) // '.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  CALL ECH_SET_CONTEXT( 'PROBLEM', ' Lost right trace' )
               END IF

*           Estimate new centre of trace using old position and slope
               y_trace = y_trace + order_slope * FLOAT( x_step )

*           If user-supplied coords have supplied an estimated trace or re-tracingthen
               IF ( user_coords .OR. re_trace ) THEN
                  y_trace = est_trace
                  right_ok = .TRUE.
               END IF
            END IF
            bad_sample = .FALSE.
         END IF
      END DO

*  Report result, and test for the case where the spectrum
*  could not be traced at all.
      DO I = 1, NX
         TRACE( I ) = ECH__BAD_REAL
      END DO

*  If not enough 'good' centres found then default to straight line
*  description of order trace.
      IF ( POINTS_TO_FIT .LT. N_POLY ) THEN
         POLYNOMIAL( 1 ) = FLOAT( ORDER_CENTRE )
         CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :         ' untraceable.'
         IF ( IAND( REPORT_MODE, RPM_FULL + RPM_ERROR ) .GT. 0 )
     :      CALL ECH_REPORT( 0, REPORT_STRING )
         CALL ECH_SET_CONTEXT( 'PROBLEM', ' Untraceable' )

*  Save the traced points for later re-fitting.
      ELSE
         DO I = 1, POINTS_TO_FIT
            IF ( INT( X_TRACE_COORD( I ) ) .GT. 0 ) THEN
               TRACE( INT( X_TRACE_COORD( I ) ) ) =
     :               REAL( Y_TRACE_COORD( I ) )
            END IF
         END DO

         IF ( AFITTER .EQ. 'IUE' ) THEN
            DO I = 1, NX
               XOFF = FLOAT( CENTRE_ROW ) - 402.
               IF ( SQRT( ABS( FLOAT( I ) - 406 ) ** 2.0 +
     :              ( ABS( XOFF ) + ORDER_SLOPE * XOFF ) ** 2.0 ) .GT.
     :              370.0 )
     :            TRACE( I ) = ECH__BAD_REAL
            END DO
         END IF

         COUNT = 0
         DO I = 1, NX
            IF ( TRACE( I ) .GT. 0.0 ) THEN
               COUNT = COUNT + 1
               Y_TRACE_COORD( COUNT ) = TRACE( I )
               X_TRACE_COORD( COUNT ) = FLOAT( I )
               INITIAL_DEV( COUNT ) = 0.5
            END IF
         END DO

*     Fit a polynomial.
         IF ( COUNT .GT. 0 )
     :      CALL ECH_FITTER( FITTER, N_POLY, TEMP_COEFFS,
     :      COUNT, X_TRACE_COORD,
     :      Y_TRACE_COORD, INITIAL_DEV, 0, 5.0, STATUS )

*     And save the coefficients
         DO I = 1, N_POLY
            POLYNOMIAL( I ) = TEMP_COEFFS( I )
         END DO

*     Evaluate the trace path from the polynomial.
         DO I = 1, MIN( NX, MAX_POINTS_PER_AXIS )
            X_COORD( I ) = FLOAT( I )
            X_TRACE_COORD( I ) = DBLE( I )
         END DO
         CALL ECH_DFEVAL( FITTER, N_POLY, POLYNOMIAL,
     :        NX, X_TRACE_COORD, Y_TRACE_COORD, STATUS )

*     Calculate initial deviations.
         DO I = 1, MIN( NX, MAX_POINTS_PER_AXIS )
            Y_COORD( I ) = REAL( Y_TRACE_COORD( I ) )
            IF ( TRACE( I ) .NE. ECH__BAD_REAL ) THEN
               INITIAL_DEV( I ) = TRACE( I ) - Y_COORD( I )

            ELSE
               INITIAL_DEV( I ) = ECH__BAD_REAL
            END IF
         END DO

*     Overlay path of initial fit to trace on image if possible.
         CALL ECH_GET_PARAMETER( 'DISPLAY', 'LOGICAL', 0.0, IMG_DISPLAY,
     :        ' ', 0, GSTATUS )
         IF ( IMG_DISPLAY ) THEN

*        Enable autoscaling if this is a new trace run.
            IF ( ORDER_NUMBER .LT. LAST_ORDER .OR.
     :           ORDER_NUMBER .EQ. 1 ) THEN
               VMIN = 0.0
               VMAX = 0.0
               LAST_ORDER = ORDER_NUMBER
            END IF

*        Pick a scaling based on region of image to be displayed.
            VMIN = 1E20
            VMAX = -1E20
            VMIN2 = 1E20
            VMAX2 = -1E20
            LOPLOT = MAX( 1, ORDER_CENTRE - 20 )
            HIPLOT = MIN( ORDER_CENTRE + 20, NY )
            DO J = 1, NX
               DO K = LOPLOT, HIPLOT
                  IF ( IMAGE( J, K ) .NE. ECH__BAD_REAL .AND.
     :                 QUALITY( J, K ) .EQ. 0 ) THEN
                     IF ( IMAGE( J, K ) .GT. VMAX ) THEN
                        VMAX2 = VMAX
                        VMAX = IMAGE( J, K )
                     END IF
                     IF ( IMAGE( J, K ) .LT. VMIN ) THEN
                        VMIN2 = VMIN
                        VMIN = IMAGE( J, K )
                     END IF
                  END IF
               END DO
            END DO
            OPTIONS = GRPH_GEN_XAXIS + GRPH_CALC_MINMAX

*        Plot the image.
            CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
            TPLOT_TITLE = ' Overlaid trace for order ' //
     :            REF_STR1( :NCHAR1 )
            CALL ECH_PLOT_GRAPH( NX * NY, IMAGE, IMAGE, 1.0,
     :           FLOAT( NX ), FLOAT( LOPLOT ), FLOAT( HIPLOT ),
     :           'X pixels',  'Y pixels', TPLOT_TITLE, VMIN2, VMAX2,
     :           OPTIONS, 'IMAGING', STATUS )

*        Set overlay colour to RED.
            CALL ECH_GR_SET_COLOUR( COL_RED )

*        Overlay the order trace path.
            OPTIONS = GRPH_OVERLAY
            CALL ECH_PLOT_GRAPH( NX, X_COORD, Y_COORD, 1.0, FLOAT( NX ),
     :           FLOAT( LOPLOT ), FLOAT( HIPLOT ), ' ', ' ', ' ',
     :           0.0, 0.0, OPTIONS, 'POINTS', STATUS )

*        Reset overlay colour to BLACK.
            CALL ECH_GR_SET_COLOUR( COL_BLACK )
         END IF

*     Report traced range to user.
         SUCCESS_RATE = INT( FLOAT( LEFT_SUCCESS + RIGHT_SUCCESS ) /
     :                  FLOAT( NX / X_STEP ) * 100.0   )
         SUCCESS_RATE = MIN( 100, SUCCESS_RATE + 1 )
         CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
         CALL CHR_ITOC( LEFT_EXTENT, REF_STR2, NCHAR2 )
         CALL CHR_ITOC( RIGHT_EXTENT, REF_STR3, NCHAR3 )
         CALL CHR_ITOC( SUCCESS_RATE, REF_STR4, NCHAR4 )
         IF ( IAND( report_mode, rpm_full + rpm_info ) .GT. 0 ) THEN
            REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :           ' traced from X=' //
     :           REF_STR2( :NCHAR2 ) // ' to ' //
     :           REF_STR3( :NCHAR3 ) //
     :           ', with a sample success rate of ' //
     :           REF_STR4( :NCHAR4 ) // '%'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
      END IF

 999  CONTINUE

      IF ( USER_COORDS ) THEN
         CALL ECH_SET_PARAMETER( 'TRACE_MODE', 'CHAR', 0.0,
     :        0, INPUT_MODE( 2: ), STATUS )
         CALL ECH_SET_PARAMETER( 'TRACE_MODE', 'CANCEL', 0.0,
     :        0, ' ', STATUS )
         CALL ECH_REPORT( 0, ' Guided re-trace processing completed.' )
      END IF

      IF ( AFITTER .NE. 'IUE' ) THEN
         AFITTER = FITTER
      END IF

      END
