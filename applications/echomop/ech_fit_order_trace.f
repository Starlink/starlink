      SUBROUTINE ECH_FIT_ORDER_TRACE(
     :           TRACE,
     :           NX,
     :           ORDER_NUMBER,
     :           MAXIMUM_POLY,
     :           AFITTER,
     :           N_POLY,
     :           POLYNOMIAL,
     :           INTERACTIVE,
     :           INITIAL_DEV,
     :           FINAL_DEV,
     :           END_CLIP_MAXDEV,
     :           AUTO_CLIP_BY,
     :           CLIPPED,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           MENU,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_FIT_ORDER_TRACE

*  Purpose:
*     Fit function to order path across frame.

*  Description:
*     This routine attempts to fit a polynomial to the trace of an order.
*     A variety of interactive/automatic clipping options are available
*     to help attain a 'good' fit.

*  Invocation:
*     CALL ECH_FIT_ORDER_TRACE(
*     :    TRACE,
*     :    NX,
*     :    ORDER_NUMBER,
*     :    MAXIMUM_POLY,
*     :    AFITTER,
*     :    N_POLY,
*     :    POLYNOMIAL,
*     :    INTERACTIVE,
*     :    INITIAL_DEV,
*     :    FINAL_DEV,
*     :    END_CLIP_MAXDEV,
*     :    AUTO_CLIP_BY,
*     :    CLIPPED,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    MENU,
*     :    STATUS
*     :   )

*  Arguments:
*     TRACE = REAL (Returned)
*        Array of NX estimates of order centre in.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     ORDER_NUMBER = INTEGER (Given)
*        Number of order being processed.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum degree of polynomial to attempt to fit.
*     AFITTER = CHAR (Given)
*        Type of fitting function to use.
*     N_POLY = INTEGER (Given)
*        Maximum degree of polynomial to attempt to fit.
*     POLYNOMIAL = DOUBLE (Returned)
*        Array of Polynomial coefficients (constant first).
*     INTERACTIVE = LOGICAL (Returned)
*        TRUE if interactive clipping is to be used.
*     INITIAL_DEV = REAL (Returned)
*        Array of initial deviations from polynomial fit.
*     FINAL_DEV = REAL (Returned)
*        Array of accepted deviations from polynomial fit.
*     END_CLIP_MAXDEV = REAL (Returned)
*        Maximum permitted deviation for autoclipping.
*     AUTO_CLIP_BY = INTEGER (Returned)
*        Number of points to clip automatically before a re-fit.
*     CLIPPED = INTEGER (Given)
*        Number of points clipped from trace set.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Workspace array for X points to be fitted.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Workspace array for Y points to be fitted.
*     MENU = LOGICAL (Given and Returned)
*        Whether user-menu for clipping should be displayed.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Authors:
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     19-MAR-1996 (MJC):
*       New prologue, tidy up.
*     11-APR-1997 (MJC):
*       More tidying.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_CONTEXT.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_MAPPING.INC'

*  Arguments:
      INTEGER NX
      REAL TRACE( NX )
      INTEGER ORDER_NUMBER ! Lowest order on image frame.
      INTEGER MAXIMUM_POLY
      CHARACTER*( * ) AFITTER
      INTEGER N_POLY
      DOUBLE PRECISION POLYNOMIAL( MAXIMUM_POLY )
      LOGICAL INTERACTIVE
      REAL INITIAL_DEV( NX )
      REAL FINAL_DEV( NX )
      REAL END_CLIP_MAXDEV
      INTEGER AUTO_CLIP_BY
      INTEGER CLIPPED
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )
      LOGICAL MENU

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )

      REAL X_GRAPH( 5000 )
      REAL Y_GRAPH( 5000 )
      REAL T_GRAPH( 5000 )
      REAL AVG
      REAL XLOW
      REAL XHI
      REAL MAX_DEVIATION
      REAL RMS_DEVIATION
      REAL PCENT
      REAL RVALUE

      INTEGER I
      INTEGER J
      INTEGER II
      INTEGER COUNT
      INTEGER POLY_DEGREE
      INTEGER SAMPLES_USED
      INTEGER IL
      INTEGER POINTS_TO_FIT
      INTEGER OPTIONS
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3
      INTEGER NCHAR4

      LOGICAL ACCEPTED
      LOGICAL ABANDONED
      LOGICAL PLOTTING
      LOGICAL TRACE_PLOT

      CHARACTER*64 TITLE
      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2
      CHARACTER*8 REF_STR3
      CHARACTER*8 REF_STR4
      CHARACTER*8 FITTER
      CHARACTER*5 PTYPE

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      DOUBLE PRECISION GEN_EPOLYD
      EXTERNAL GEN_EPOLYD
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( ECH__MOD_ENTRY,  'ECH_FIT_ORDER_TRACE' )

      ACCEPTED = .FALSE.
      ABANDONED = .FALSE.
      TRACE_PLOT = .FALSE.
      PLOTTING = .TRUE.
      RMS_DEVIATION = 0.5
      CLIPPED = 0

*  We will use the existing fit...
      FITTER = '?'

*  Unless these are IUE data.
      IF ( AFITTER .EQ. 'IUE' ) THEN
         FITTER = 'POLY'
         IL = LEN( CSTR_RDCTN_TRACIM )
         DO WHILE ( IL .GT. 1 .AND. CSTR_RDCTN_TRACIM(IL:IL) .EQ. ' ' )
            IL = IL  - 1
         END DO
         IF ( CSTR_RDCTN_TRACIM( IL-7:IL-5 ) .EQ. 'SWP' .OR.
     :        CSTR_RDCTN_TRACIM( IL-7:IL-5 ) .EQ. 'LWP' .OR.
     :        CSTR_RDCTN_TRACIM( IL-7:IL-5 ) .EQ. 'LWR' ) THEN
            USR_TUNE_IUE = 1
         END IF
      END IF

      CALL ECH_FIND_POLY( FITTER, POLYNOMIAL, N_POLY, MAXIMUM_POLY,
     :     POLY_DEGREE )

*  Loop determining the polynomial coefficients until exit.
  100 DO WHILE ( .NOT. ACCEPTED .AND. .NOT. ABANDONED )

*     Copy any good points coordinates into work arrays for fitting.
         POINTS_TO_FIT = 0
         DO I = 1, NX
            IF ( TRACE( I ) .GT. 0.0 ) THEN
               POINTS_TO_FIT = POINTS_TO_FIT + 1
               Y_TRACE_COORD( POINTS_TO_FIT ) = TRACE( I )
               X_TRACE_COORD( POINTS_TO_FIT ) = FLOAT( I )
               Y_GRAPH( POINTS_TO_FIT ) = RMS_DEVIATION
            END IF
         END DO
         SAMPLES_USED = MAX( SAMPLES_USED, POINTS_TO_FIT )

*     Do the polynomial fitting.
         IF ( POINTS_TO_FIT .GT. N_POLY ) THEN
            DO I = 1, MAXIMUM_POLY
               TEMP_COEFFS( I ) = 0.0
            END DO
            CALL ECH_FITTER( FITTER, POLY_DEGREE, TEMP_COEFFS,
     :           POINTS_TO_FIT, X_TRACE_COORD, Y_TRACE_COORD,
     :           Y_GRAPH, 0, 5.0, STATUS )

         ELSE
            ABANDONED = .TRUE.
            CALL ECH_REPORT ( 0,
     :           ' Rejecting fit: Not enough samples left to fit.' )
            GO TO 100
         END IF

         IF ( AFITTER .EQ. 'IUE' ) THEN
            CALL ECH_FEVAL( FITTER, POLY_DEGREE, TEMP_COEFFS,
     :           1, 406.0, RVALUE, STATUS )
            IF ( N_POLY .EQ. 1 ) THEN
               DO I = 1, NX
                  IF ( SQRT( ABS( FLOAT( I ) - 380.0 ) ** 2.0 +
     :                 ABS( RVALUE - 375.0 ) ** 2.0 ) .GT. 360.0 )
     :               TRACE( I ) = ECH__BAD_REAL
               END DO

            ELSE
               DO I = 1, NX
                  IF ( SQRT( ABS( FLOAT( I ) - 406.0 ) ** 2.0 +
     :                 ABS( RVALUE - 402.0 ) ** 2.0 ) .GT. 370.0 )
     :               TRACE( I ) = ECH__BAD_REAL
               END DO
            END IF
         END IF

*     Calculate deviations from fit.
         MAX_DEVIATION = 0.0
         RMS_DEVIATION = 0.0
         COUNT = 0
         DO I = 1, NX
           IF ( TRACE( I ) .NE. ECH__BAD_REAL ) THEN
               COUNT = COUNT + 1
               X_TRACE_COORD( I ) = FLOAT( I )
               X_GRAPH( COUNT ) = FLOAT( I )
               CALL ECH_FEVAL( FITTER, POLY_DEGREE, TEMP_COEFFS,
     :              1, X_GRAPH(COUNT), RVALUE, STATUS )
               FINAL_DEV( I ) = TRACE( I ) - RVALUE
               Y_GRAPH( COUNT ) = FINAL_DEV( I )
               IF ( ABS( FINAL_DEV( I ) ) .GT. MAX_DEVIATION )
     :            MAX_DEVIATION = ABS( FINAL_DEV( I ) )
               RMS_DEVIATION = RMS_DEVIATION +
     :            ABS( FINAL_DEV( I ) ) * ABS( FINAL_DEV( I ) )

            ELSE
               FINAL_DEV( I ) = ECH__BAD_REAL
            END IF
         END DO

*     Determine RMS, max deviation, set acceptance if fit good enough.
         RMS_DEVIATION = SQRT( RMS_DEVIATION / FLOAT( COUNT ) )
         IF ( MAX_DEVIATION .LE. END_CLIP_MAXDEV )
     :      ACCEPTED = .TRUE.

*     Plot if enabled or automatically accepted.
         XLOW = 1.0
         XHI = FLOAT( NX )
         IF ( ( INTERACTIVE .OR. ACCEPTED ) .AND.
     :        POINTS_TO_FIT .GT. N_POLY ) THEN
            OPTIONS = GRPH_CALC_YMINMAX
            PTYPE = '+'
            IF ( USR_TUNE_IUE .GT. 0 ) THEN
               PTYPE = 'LINES'
               CALL CHR_ITOC( 66 + 55 - ORDER_NUMBER, REF_STR1,
     :              NCHAR1 )
               TITLE = ' IUE Hires Order ' // REF_STR1( :NCHAR1 ) //
     :               ': Image ' // CSTR_RDCTN_TRACIM( IL - 7 : IL )

            ELSE IF ( AFITTER .EQ. 'IUE' ) THEN
               PTYPE = 'LINES'
               CALL CHR_ITOC( 66 + 55 - ORDER_NUMBER, REF_STR1,
     :              NCHAR1 )
               CALL CHR_ITOC( POINTS_TO_FIT, REF_STR2, NCHAR2 )
               CALL CHR_ITOC( CLIPPED, REF_STR3, NCHAR3 )
               CALL CHR_ITOC( POLY_DEGREE - 1, REF_STR4, NCHAR4 )
               TITLE = 'IUE Order ' // REF_STR1( :NCHAR1 ) //
     :               ': samples=' // REF_STR2( :NCHAR2 ) //
     :               ', clipped=' // REF_STR3( :NCHAR3 ) //
     :               ', degree=' // REF_STR4( :NCHAR4 )

            ELSE
               CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
               CALL CHR_ITOC( POINTS_TO_FIT, REF_STR2, NCHAR2 )
               CALL CHR_ITOC( CLIPPED, REF_STR3, NCHAR3 )
               TITLE = 'Order ' // REF_STR1( :NCHAR1 ) //
     :               ': samples=' // REF_STR2( :NCHAR2 ) //
     :               ', clipped=' // REF_STR3( :NCHAR3 )
               IF ( FITTER .EQ. 'SPLINE' ) THEN
                  CALL CHR_ITOC( POLY_DEGREE / 2 - 7, REF_STR4,
     :                 NCHAR4 )
                  TITLE = TITLE( :CHR_LEN( TITLE ) ) //
     :                  ', knots=' // REF_STR4( :NCHAR4 )

               ELSE
                  CALL CHR_ITOC( POLY_DEGREE - 1, REF_STR4, NCHAR4 )
                  TITLE = TITLE( :CHR_LEN( TITLE ) ) //
     :                  ', degree=' // REF_STR4( :NCHAR4 )
               END IF
            END IF
            IF ( AFITTER .EQ. 'IUE' ) THEN
               DO II = 1, 3
                  DO I = 1, COUNT
                     AVG = 0.0
                     DO J = -15, 15
                        AVG = AVG + Y_GRAPH( MIN( COUNT,
     :                        MAX( 1, I + J ) ) )
                     END DO
                     T_GRAPH(I) = AVG / 31.0
                  END DO
                  DO I = 1, COUNT
                     Y_GRAPH( I ) = T_GRAPH( I )
                  END DO
               END DO
               XLOW = X_GRAPH( 1 )
               XHI = X_GRAPH( COUNT )
               POINTS_TO_FIT = COUNT
            END IF

*        Plot deviations.
            IF ( .NOT. TRACE_PLOT ) THEN
               IF ( PLOTTING )
     :            CALL ECH_PLOT_GRAPH( POINTS_TO_FIT, X_GRAPH,
     :                 Y_GRAPH, XLOW, XHI, 0.0, 0.0, 'X pixel',
     :                 'Deviation in pixels', TITLE, 0.0, 1.1,
     :                 OPTIONS, PTYPE, STATUS )

*         Plot trace and fitted curve.
            ELSE
               COUNT = 0
               DO I = 1, NX
                 IF ( TRACE( I ) .NE. ECH__BAD_REAL ) THEN
                  COUNT = COUNT + 1
                  X_GRAPH( COUNT ) = FLOAT( I )
                  Y_GRAPH( COUNT ) = TRACE( I )
                 END IF
               END DO
               OPTIONS = GRPH_CALC_YMINMAX

*           Plot trace.
               IF ( PLOTTING )
     :            CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH, Y_GRAPH, 1.0,
     :                 FLOAT( NX ), 0.0, 0.0, 'X pixel', 'Y pixel',
     :                 TITLE, 0.0, 0.0, OPTIONS, 'POINTS', STATUS )
               COUNT = 0

*           Generate points on fitted curve and plot.
               DO I = 3, NX, MAX( 1, NX/200 )
                  COUNT = COUNT + 1
                  X_GRAPH( COUNT ) = FLOAT( I )
               END DO
               CALL ECH_FEVAL( FITTER, POLY_DEGREE, TEMP_COEFFS,
     :              COUNT, X_GRAPH, Y_GRAPH, STATUS )
               OPTIONS = GRPH_OVERLAY + GRPH_SET_COLOUR
               IF ( PLOTTING )
     :            CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH, Y_GRAPH,
     :                 0.0, 0.0, 0.0, 0.0, 'RED', ' ', ' ',
     :                 0.0, 0.0, OPTIONS, 'LINES', STATUS )
            END IF
         END IF

*     Clip some more points from trace.
         CALL ECH_CLIP_TRACE( NX, ORDER_NUMBER, MAXIMUM_POLY,
     :        FITTER, INTERACTIVE, TRACE_PLOT, END_CLIP_MAXDEV,
     :        AUTO_CLIP_BY, POLY_DEGREE, CLIPPED, TRACE, FINAL_DEV,
     :        PLOTTING, MENU, ABANDONED, ACCEPTED, STATUS )
      END DO

*  Save resulting polynomial.
      IF ( STATUS .EQ. ECH__ABORT_OPTION ) THEN
         REPORT_STRING = ' Fitting aborted by user request.'
         STATUS = 0

      ELSE IF ( ACCEPTED .AND. .NOT. ABANDONED ) THEN
         DO I = 1, POLY_DEGREE
            POLYNOMIAL( I ) = TEMP_COEFFS( I )
         END DO
         DO I = POLY_DEGREE + 1, MAXIMUM_POLY
            POLYNOMIAL( I ) = 0.0
         END DO
         CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
         CALL CHR_RTOC( REAL( INT( 1000.0 * MAX_DEVIATION ) ) / 1000.0,
     :        REF_STR2, NCHAR2 )
         CALL CHR_RTOC( REAL( INT( 1000.0 * RMS_DEVIATION ) ) / 1000.0,
     :        REF_STR3, NCHAR3 )
         REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :         ' trace fit residuals: max=' //
     :         REF_STR2( :NCHAR2 ) // ' RMS=' //
     :         REF_STR3( :NCHAR3 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         PCENT = FLOAT( NINT( FLOAT( CLIPPED * 1000 ) /
     :         FLOAT( SAMPLES_USED ) ) ) / 10.0
         CALL CHR_RTOC( PCENT, REF_STR2, NCHAR2 )
         REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) // ' ' //
     :         REF_STR2( :NCHAR2 ) // '% samples clipped.'

      ELSE IF ( ABANDONED .AND. .NOT. ACCEPTED ) THEN
         DO I = 1, MAXIMUM_POLY
            POLYNOMIAL( I ) = ECH__BAD_DOUBLE
         END DO
         CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :         '   abandoned: too many points clipped.'
      END IF
      CALL ECH_REPORT( 0, REPORT_STRING )

      END


      SUBROUTINE ECH_FIND_POLY( FITFN, POLY, N_POLY, MAX_POLY, DEGREE )

      IMPLICIT NONE

      CHARACTER*( * ) FITFN
      INTEGER N_POLY
      INTEGER MAX_POLY
      DOUBLE PRECISION POLY( MAX_POLY )
      INTEGER DEGREE

      INTEGER I

      DEGREE = 0
      DO I = 1, MAX_POLY
         IF ( POLY( I ) .NE. 0.0 ) THEN
            DEGREE = I
         END IF
      END DO
      IF ( N_POLY .LE. 0 ) THEN
         N_POLY = DEGREE
      END IF

      IF ( FITFN .EQ. '?' ) THEN
         IF ( DEGREE .GT. 11 ) FITFN = 'SPLINE'
         IF ( DEGREE .LE. 11 ) FITFN = 'POLY'
      END IF
      IF ( FITFN .EQ. 'POLY' .AND.
     :     ( DEGREE .LT. 1 .OR. 11 .LT. DEGREE ) ) THEN
         DEGREE = N_POLY
      END IF
      IF ( FITFN .EQ. 'SPLINE' ) THEN
         DEGREE = DEGREE + 4
      END IF

      END
