      SUBROUTINE ECH_PLOT_GRAPH(
     :           INP_NO_OF_POINTS,
     :           X_DATA,
     :           Y_DATA,
     :           INP_XMIN,
     :           INP_XMAX,
     :           INP_YMIN,
     :           INP_YMAX,
     :           X_LABEL,
     :           Y_LABEL,
     :           TITLE,
     :           INP_X_AXIS_SCALING,
     :           INP_Y_AXIS_SCALING,
     :           OPTIONS,
     :           FORMAT,
     :           STATUS
     :          )
*+
*  Name:
*     ECH_PLOT_GRAPH

*  Purpose:
*     This routine plot graphs for most other modules.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ECH_PLOT_GRAPH(
*     :    INP_NO_OF_POINTS,
*     :    X_DATA,
*     :    Y_DATA,
*     :    INP_XMIN,
*     :    INP_XMAX,
*     :    INP_YMIN,
*     :    INP_YMAX,
*     :    X_LABEL,
*     :    Y_LABEL,
*     :    TITLE,
*     :    INP_X_AXIS_SCALING,
*     :    INP_Y_AXIS_SCALING,
*     :    OPTIONS,
*     :    FORMAT,
*     :    STATUS
*     :   )

*  Arguments:
*    INP_NO_OF_POINTS = INTEGER (Given)
*       Number of points to plot.
*    X_DATA = REAL (Given)
*       Array of data point for fit/plot.
*    Y_DATA = DOUBLE (Given)
*       Y data to plot.
*    INP_XMIN = REAL (Given)
*       Minimum X value to plot.
*    INP_XMAX = REAL (Given)
*       Maximum X value to plot.
*    INP_YMIN = REAL (Given)
*       Minimum Y value to plot.
*    INP_YMAX = REAL (Given)
*       Maximum Y value to plot.
*    X_LABEL = CHAR (Given)
*       Label for X-axis.
*    Y_LABEL = CHAR (Given)
*       Label for Y-axis.
*    TITLE = CHAR (Given)
*       Title text.
*    INP_X_AXIS_SCALING = REAL (Given)
*       Scaling factor for X axis.
*    INP_Y_AXIS_SCALING = REAL (Given)
*       Scaling factor for Y axis.
*    OPTIONS = INTEGER (Given)
*       Mask containing plotting options.
*    FORMAT = CHAR (Given)
*       Style of plot (e.g. line, bin, etc.).
*    STATUS = INTEGER (Given and Returned)
*       The global status.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     26-MAR-1997 (MJC):
*       Added prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_SERVER.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments:
      INTEGER INP_NO_OF_POINTS
      REAL INP_X_AXIS_SCALING
      REAL INP_Y_AXIS_SCALING
      REAL INP_XMIN
      REAL INP_XMAX
      REAL INP_YMIN
      REAL INP_YMAX
      REAL X_DATA( INP_NO_OF_POINTS )
      REAL Y_DATA( INP_NO_OF_POINTS )
      CHARACTER*( * ) FORMAT
      CHARACTER*( * ) X_LABEL
      CHARACTER*( * ) Y_LABEL
      CHARACTER*( * ) TITLE
      INTEGER OPTIONS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAX_COLOURS
      PARAMETER ( MAX_COLOURS = 8 )

      INTEGER MAX_FONTS
      PARAMETER ( MAX_FONTS = 4 )

      INTEGER MAX_LINESTYLES
      PARAMETER ( MAX_LINESTYLES = 5 )

*  Local Variables:
      REAL X_PLOTTED( MAX_POINTS_PER_AXIS )
      REAL Y_PLOTTED( MAX_POINTS_PER_AXIS )
      REAL TFORM( 6 )
      REAL XMAX
      REAL YMIN
      REAL YMAX
      REAL VMIN
      REAL VMAX
      REAL XMIN
      REAL VPXMIN
      REAL VPSCALE
      REAL VPXMAX
      REAL VPYMIN
      REAL VPYMAX
      REAL X_AXIS_SCALING
      REAL Y_AXIS_SCALING
      REAL TEMP

      INTEGER I
      INTEGER NO_OF_POINTS
      INTEGER ISTAT
      INTEGER STEP
      INTEGER INDEX
      INTEGER GLUN
      INTEGER IBAD
      INTEGER ILEN
      INTEGER IFONT
      INTEGER DEF_WID
      INTEGER DEF_COL
      INTEGER DEF_LIN
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER I_SYMBOL

      CHARACTER*255 ONAME
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*3 ACUR

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER PGBEG
      INTEGER ECH_WORD_LEN

*  Data Statements:
      DATA TFORM / 0., 1., 0., 0., 0., 1. /
      DATA DEF_WID / 1 /
      DATA DEF_COL / 1 /
      DATA DEF_LIN / 1 /
      INCLUDE 'ECH_PLOT_PARS.INC'
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

      STATUS = SAI__OK

*  Start wrapper error context to catch any PGPLOT errors.
      CALL ERR_MARK

*  Handle basic operations - close device.
      IF ( OPTIONS .EQ. GRPH_CLOSE_DEV ) THEN
         GRAPHICS_SETUP = .FALSE.
         CALL PGEND
         GO TO 999
      END IF

*  Open device.
      IF ( OPTIONS .EQ. GRPH_OPEN_DEV ) THEN
         ISTAT = PGBEG( 0, FORMAT, INT( INP_XMIN ), INT( INP_XMAX ) )
         IF ( ISTAT .EQ. 1 ) THEN
            GRAPHICS_SETUP = .TRUE.
            CALL PGQINF( 'CURSOR', ACUR, ILEN )
            IPGCUR = 0
            IF ( ACUR .EQ. 'YES' ) IPGCUR = 1

         ELSE
            STATUS = ISTAT
         END IF
         GO TO 999
      END IF

*  When in server mode...
      IF ( USR_TUNE_SERVER ) THEN
         GO TO 998
      END IF

      IF ( .NOT. GRAPHICS_SETUP ) CALL ECH_SETUP_GRAPHICS( STATUS )
      IF ( FORMAT .EQ. 'IMAGE-OVERLAY' .AND. .NOT. IMG_DISPLAY ) THEN
         GO TO 999

      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 998
      END IF

*  Open hardcopy device.
      IF ( OPTIONS .EQ. GRPH_HARDCOPY ) THEN
         REPORT_STRING = ' Sending hardcopy to device ' //
     :        HARDCOPY_DEVICE_NAME
         CALL ECH_REPORT( 0, REPORT_STRING )
         CALL PGEND
         ISTAT = PGBEG( 0, HARDCOPY_DEVICE_NAME, 1, 1 )
         IF ( ISTAT .NE. 1 ) THEN
            STATUS = ISTAT
            GO TO 998
         END IF
      END IF

*  ASCII dump of graphic data.
      IF ( IAND( OPTIONS, GRPH_DUMP_ASCII ) .NE. 0 ) THEN
         ISTAT = SAI__OK
         CALL ECH_OPEN_FILE( Y_LABEL, 'TEXT', 'NEW', .TRUE.,
     :        GLUN, ONAME, ISTAT )
         WRITE ( GLUN, '( 1X, A, A )' )
     :         'Data from: ', TITLE( :ECH_WORD_LEN( TITLE ) )
         WRITE ( GLUN, '( 1X, A )' )
     :         'Suitable for reading with DIPSO SP2RD.'
         WRITE ( GLUN, '( 1X, I8 )' ) INP_NO_OF_POINTS
         DO I = 1, INP_NO_OF_POINTS
           IF ( IAND( OPTIONS, GRPH_GEN_XAXIS ) .EQ.
     :          GRPH_GEN_XAXIS ) THEN
              WRITE ( GLUN, X_LABEL, ERR=103 ) FLOAT( I ), Y_DATA( I )

           ELSE IF ( IAND( OPTIONS, GRPH_GEN_YAXIS ) .EQ.
     :               GRPH_GEN_YAXIS ) THEN
              WRITE ( GLUN, X_LABEL, ERR=103 ) X_DATA( I ), FLOAT( I )

           ELSE
              WRITE ( GLUN, X_LABEL, ERR=103 ) X_DATA( I ), Y_DATA( I )
           END IF
  103      CONTINUE
         END DO
         CALL ECH_OPEN_FILE( ' ', 'CLOSE', ' ', .TRUE., GLUN, ONAME,
     :        ISTAT )
         REPORT_STRING = ' Plot ASCII-dumped to file ' //
     :         Y_LABEL( :ECH_WORD_LEN( Y_LABEL ) ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         GO TO 999
      END IF

      NO_OF_POINTS = INP_NO_OF_POINTS
      STEP = 1
      XMIN = INP_XMIN
      XMAX = INP_XMAX
      YMIN = INP_YMIN
      YMAX = INP_YMAX
      X_AXIS_SCALING = INP_X_AXIS_SCALING
      Y_AXIS_SCALING = INP_Y_AXIS_SCALING


*  1-D data plotting, determine range limits
*  =========================================

      IF ( FORMAT .NE. 'IMAGING' ) THEN
         IF ( NO_OF_POINTS .GT. MAX_POINTS_PER_AXIS ) THEN
            STEP = INP_NO_OF_POINTS / MAX_POINTS_PER_AXIS + 1
            NO_OF_POINTS = INP_NO_OF_POINTS / STEP
         END IF

*     X-axis.
         IF ( IAND( OPTIONS, GRPH_GEN_XAXIS ) .NE. 0 ) THEN
            DO I = STEP, NO_OF_POINTS, STEP
               X_PLOTTED( I ) = FLOAT( I )
            END DO

         ELSE
            DO I = STEP, NO_OF_POINTS, STEP
               X_PLOTTED( I ) = X_DATA( I )
            END DO
         END IF

*     Y-axis.
         IF ( IAND( OPTIONS, GRPH_GEN_YAXIS ) .NE. 0 ) THEN
            DO I = STEP, NO_OF_POINTS, STEP
               Y_PLOTTED( I ) = FLOAT( I )
            END DO

         ELSE
            DO I = STEP, NO_OF_POINTS, STEP
               Y_PLOTTED( I ) = Y_DATA( I )
            END DO
         END IF

*     Find X-range of data.
         IF ( ( IAND( OPTIONS, GRPH_CALC_XMINMAX ) .NE. 0 ) .OR.
     :        ( IAND( OPTIONS, GRPH_CALC_MINMAX ) .NE. 0 ) ) THEN
            XMIN = 1.0E20
            XMAX = -1.0E20
            DO I = 1, NO_OF_POINTS
               IF ( X_PLOTTED( I ) .NE. ECH__BAD_REAL ) THEN
                  IF ( X_PLOTTED( I ) .LT. XMIN .AND.
     :                 X_PLOTTED( I ) .NE. 0.0 )
     :               XMIN = X_PLOTTED( I )
                  IF ( X_PLOTTED( I ) .GT. XMAX ) XMAX = X_PLOTTED( I )
               END IF
            END DO
            IF ( XMIN .EQ. XMAX ) THEN
               XMIN = XMIN - 1.0
               XMAX = XMAX + 1.0
               CALL ECH_REPORT( 0, ' X-axis values are constant.' )
            END IF
         END IF

*     Find Y-range of data.
         IF ( ( IAND( OPTIONS, GRPH_CALC_YMINMAX ) .NE. 0 ) .OR.
     :        ( IAND( OPTIONS, GRPH_CALC_MINMAX ) .NE. 0 ) ) THEN
            YMIN = 1.0E20
            YMAX = -1.0E20
            DO I = 1, NO_OF_POINTS
               IF ( Y_PLOTTED( I ) .NE. ECH__BAD_REAL ) THEN
                  IF ( Y_PLOTTED( I ) .LT. YMIN ) YMIN = Y_PLOTTED( I )
                  IF ( Y_PLOTTED( I ) .GT. YMAX ) YMAX = Y_PLOTTED( I )
               END IF
            END DO
            IF ( INP_YMAX .EQ. 2.0 ) THEN
               YMIN = YMIN - ( YMAX - YMIN ) / 2.0
               YMAX = YMAX + ( YMAX - YMIN ) / 2.0
            END IF
            IF ( YMIN .EQ. YMAX ) THEN
               YMIN = YMIN - 1.0
               YMAX = YMAX + 1.0
               CALL ECH_REPORT( 0, ' Y-axis values are constant.' )
            END IF
         END IF

*     Scale axes.
         IF ( X_AXIS_SCALING .NE. 0.0 ) THEN
            XMIN = XMIN * X_AXIS_SCALING
            XMAX = XMAX * X_AXIS_SCALING
         END IF
         IF ( Y_AXIS_SCALING .NE. 0.0 ) THEN
            YMIN = YMIN * Y_AXIS_SCALING
            YMAX = YMAX * Y_AXIS_SCALING
         END IF


*  2-D data plotting - find Z limits
*  =================================

      ELSE

*     Determine suitable limits for display.
         IF ( INP_X_AXIS_SCALING .EQ. 0.0 .AND.
     :        INP_Y_AXIS_SCALING .EQ. 0.0 ) THEN
            VMIN = 1.0E20
            VMAX = -1.0E20
            DO I = 1, NO_OF_POINTS
               IF ( Y_DATA( I ) .NE. ECH__BAD_REAL ) THEN
                  IF ( Y_DATA( I ) .LT. VMIN ) VMIN = Y_DATA( I )
                  IF ( Y_DATA( I ) .GT. VMAX ) VMAX = Y_DATA( I )
               END IF
            END DO
            IF ( VMIN .EQ. VMAX ) THEN
               VMIN = VMIN - 1.0
               VMAX = VMAX + 1.0
               CALL ECH_REPORT( 0, ' Data values are constant.' )

            ELSE
               CALL CHR_RTOC( VMIN, REF_STR1, NCHAR1 )
               CALL CHR_RTOC( VMAX, REF_STR2, NCHAR2 )
               REPORT_STRING = ' Data range from ' //
     :               REF_STR1( :NCHAR1 ) // ' to ' //
     :               REF_STR2( :NCHAR2 ) // '.'
               CALL ECH_REPORT( 0, REPORT_STRING )
            END IF
            INP_X_AXIS_SCALING = VMIN
            INP_Y_AXIS_SCALING = VMAX

         ELSE
            VMIN = INP_X_AXIS_SCALING
            VMAX = INP_Y_AXIS_SCALING
            CALL CHR_RTOC( VMIN, REF_STR1, NCHAR1 )
            CALL CHR_RTOC( VMAX, REF_STR2, NCHAR2 )
            REPORT_STRING = ' Data range (user-specified) from ' //
     :               REF_STR1( :NCHAR1 ) // ' to ' //
     :               REF_STR2( :NCHAR2 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
      END IF

*  Requesting of one or more of these various functions
*  implies that we don't need a set of axes.  So generate
*  the axes if no request is present.
      IF ( IAND( OPTIONS, GRPH_OVERLAY ) .EQ. 0 .AND.
     :     IAND( OPTIONS, GRPH_SET_COLOUR ) .EQ. 0 .AND.
     :     IAND( OPTIONS, GRPH_SET_FONT ) .EQ. 0 .AND.
     :     IAND( OPTIONS, GRPH_SET_PROMPT ) .EQ. 0 .AND.
     :     IAND( OPTIONS, GRPH_SET_NOPROMPT ) .EQ. 0 .AND.
     :     IAND( OPTIONS, GRPH_SET_WIDTH ) .EQ. 0 .AND.
     :     IAND( OPTIONS, GRPH_SET_LINE_STYLE ) .EQ. 0 .AND.
     :     ( FORMAT .NE. 'IMAGE-OVERLAY' ) ) THEN

*     Axes in solid, black lines.
         CALL ECH_GR_SET_COLOUR( 1 )
         CALL ECH_GR_SET_LINESTYLE( 1 )

*     Get window limits if requested.
         IF ( IAND( OPTIONS, GRPH_WINDOW ) .NE. 0 ) THEN
            IF ( IAND( OPTIONS, GRPH_USER_WINDOW ) .NE. 0 ) THEN
               CALL PGSVP( 0.0, 1.0, 0.0, 1.0 )
               CALL PGSWIN( 0.0, 1.0, 0.0, 1.0 )
               CALL ECH_REPORT( 0,
     :              ' Click mouse at lower-left limit.' )
               CALL ECH_READ_GRPH_CURSOR( STATUS )
               VPXMIN = X_CURSOR
               VPYMIN = Y_CURSOR
               CALL ECH_REPORT( 0,
     :              ' Click mouse at upper-right limit.' )
               CALL ECH_READ_GRPH_CURSOR( STATUS )
               VPXMAX = X_CURSOR
               VPYMAX = Y_CURSOR
               IF ( VPXMAX .LT. VPXMIN ) THEN
                  TEMP = VPXMIN
                  VPXMIN = VPXMAX
                  VPXMAX = TEMP
               END IF
               IF ( VPYMAX .LT. VPYMIN ) THEN
                  TEMP = VPYMIN
                  VPYMIN = VPYMAX
                  VPYMAX = TEMP
               END IF
               VPSCALE = VPXMAX - VPXMIN
               CALL PGSCH( VPSCALE )
               CALL PGSVP( VPXMIN + 0.05 * VPSCALE,
     :              VPXMAX - 0.01 * VPSCALE,
     :              VPYMIN + 0.08 * VPSCALE,
     :              VPYMAX - 0.065 * VPSCALE )
            ELSE
               CALL PGSCH( 1.0 )
            END IF
            CALL PGSWIN( XMIN, XMAX, YMIN, YMAX )

         ELSE
            CALL PGSCH( 1.0 )
            CALL PGENV( XMIN, XMAX, YMIN, YMAX, 0, -1 )
         END IF

*     Put tick marks on outside of box for 2-D data.
         IF ( FORMAT .EQ. 'IMAGING' ) THEN
            CALL PGBOX( 'ABCNSTI', 0.0, 0, 'ABCNSTI', 0.0, 0 )

         ELSE
            CALL PGBOX( 'ABCNST', 0.0, 0, 'ABCNST', 0.0, 0 )
         END IF

*     Label the plot.
         CALL PGLAB( X_LABEL, Y_LABEL, TITLE )

*     Set colour, linestyle, and line width to defaults.
         CALL ECH_GR_SET_COLOUR( DEF_COL )
         CALL ECH_GR_SET_LINESTYLE( DEF_LIN )
         CALL PGSLW( DEF_WID )

*  Control of some style parameter was requested.
      ELSE

*     Set prompt.
         IF ( IAND( OPTIONS, GRPH_SET_PROMPT ) .NE. 0 ) THEN
            CALL PGASK( .TRUE. )
         END IF

*     Set no prompt.
         IF ( IAND( OPTIONS, GRPH_SET_NOPROMPT ) .NE. 0 ) THEN
            CALL PGASK( .FALSE. )
         END IF

*     Set width.
         IF ( IAND( OPTIONS, GRPH_SET_WIDTH ) .NE. 0 ) THEN
            I = MIN( 201, MAX( INT( INP_XMIN ), 1 ) )
            CALL PGSLW( I )
            IF ( OPTIONS .EQ. GRPH_SET_WIDTH ) THEN
               DEF_WID = I
               GO TO 999
            END IF
         END IF

*     Set font.
         IF ( IAND( OPTIONS, GRPH_SET_FONT ) .NE. 0 ) THEN
            ILEN = LEN( X_LABEL )
            DO I = 1, MAX_FONTS
               IF ( X_LABEL .EQ. FONTS( I )( :ILEN ) ) THEN
                  CALL PGSCF( IFONT )
		  WRITE ( 6, * ) 'ni!', ifont
                  IF ( OPTIONS .EQ. GRPH_SET_FONT ) THEN
                     GO TO 999
                  END IF
               END IF
            END DO
         END IF

*     Set colour.
         IF ( IAND( OPTIONS, GRPH_SET_COLOUR ) .NE. 0 ) THEN
            ILEN = LEN( X_LABEL )
            DO I = 1, MAX_COLOURS
               IF ( X_LABEL .EQ. COLOURS( I )( :ILEN ) ) THEN
                  CALL ECH_GR_SET_COLOUR( I )
                  IF ( OPTIONS .EQ. GRPH_SET_COLOUR ) THEN
                     DEF_COL = I
                     GO TO 999
                  END IF
               END IF
            END DO
         END IF

*     Set line style.
         IF ( IAND( OPTIONS, GRPH_SET_LINE_STYLE ) .NE. 0 ) THEN
            ILEN = LEN( Y_LABEL )
            DO I = 1, MAX_LINESTYLES
               IF ( Y_LABEL .EQ. LINE_STYLES( I )( :ILEN ) ) THEN
                  CALL ECH_GR_SET_LINESTYLE( I )
                  IF ( OPTIONS .EQ. GRPH_SET_LINE_STYLE ) THEN
                     DEF_LIN = I
                     GO TO 999
                  END IF
               END IF
            END DO
         END IF
      END IF

*  Remove BAD points from "spectrum" plots.
      IF ( FORMAT .NE. 'IMAGING' ) THEN
         IBAD = NO_OF_POINTS
         CALL ECH_PLOTTER_REMBAD( X_PLOTTED, Y_PLOTTED,
     :        NO_OF_POINTS )
         IBAD = IBAD - NO_OF_POINTS
         IF ( IBAD .GT. 0 ) THEN
            CALL CHR_ITOC( IBAD, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Unable to plot ' //
     :            REF_STR1( :NCHAR1 ) // 'bad points.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
      END IF

*  Actually do the plot.
      IF ( FORMAT .EQ. 'IMAGING' ) THEN
         CALL PGGRAY( Y_DATA, INT( INP_XMAX ), INT( INP_YMAX ),
     :        INT( INP_XMIN ), INT( INP_XMAX ), INT( INP_YMIN ),
     :        INT( INP_YMAX ), VMIN, VMAX, TFORM )
         CALL PGSWIN( INP_XMIN, INP_XMAX, INP_YMIN, INP_YMAX )

      ELSE IF ( FORMAT .EQ. 'POINTS' ) THEN
         CALL PGPT( NO_OF_POINTS, X_PLOTTED, Y_PLOTTED, -1 )

      ELSE IF ( FORMAT .EQ. 'BINS' ) THEN
         CALL PGBIN( NO_OF_POINTS, X_PLOTTED, Y_PLOTTED, .TRUE. )

      ELSE IF ( FORMAT .EQ. 'LINES' ) THEN
         CALL PGLINE( NO_OF_POINTS, X_PLOTTED, Y_PLOTTED )

      ELSE IF ( FORMAT .EQ. 'TEXT' ) THEN
         CALL PGPTXT( X_PLOTTED, Y_PLOTTED,  90.0, 0.0, TITLE )

      ELSE IF ( FORMAT .NE. 'IMAGE-OVERLAY' ) THEN
         I_SYMBOL = ICHAR( FORMAT( :1 ) )
         CALL PGPT( NO_OF_POINTS, X_PLOTTED, Y_PLOTTED, I_SYMBOL )
      END IF

*  Examine the plot using the cursor.
      IF ( IAND( OPTIONS, GRPH_LOOP_EXAMINE ) .GT. 0 ) THEN
         CALL ECH_REPORT( 0, ' Type Q to quit cursor interaction.' )
         CALL ECH_READ_GRPH_CURSOR( STATUS )
         DO WHILE ( USER_INPUT_CHAR .NE. 'Q' )
            INDEX = 1
            DO WHILE ( X_PLOTTED( INDEX ) .LT. X_CURSOR .AND.
     :                 INDEX .LT. NO_OF_POINTS )
               INDEX = INDEX + 1
            END DO
            DO I = MAX( 1, INDEX - 2 ),
     :             MIN( INDEX + 2, NO_OF_POINTS )
               WRITE ( REPORT_STRING, 900 )
     :               X_PLOTTED( I ), Y_PLOTTED( I )
  900          FORMAT ( 1X, 'X=', F12.3, ' Y=', F12.3 )
               CALL ECH_REPORT( 0, REPORT_STRING )
            END DO
            CALL CHR_RTOC( X_CURSOR, REF_STR1, NCHAR1 )
            CALL CHR_RTOC( Y_CURSOR, REF_STR2, NCHAR2 )
            REPORT_STRING = ' Cursor at (' //
     :            REF_STR1( :NCHAR1 ) // ',' //
     :            REF_STR2( :NCHAR2 ) // ').'
            CALL ECH_REPORT( 0, REPORT_STRING )
            CALL ECH_REPORT( 0,' Type Q to quit.' )
            CALL ECH_REPORT( 0,' ' )
            CALL ECH_READ_GRPH_CURSOR( STATUS )
         END DO
      END IF

*  Close hardcopy device and re-open softcopy.
  998 IF ( OPTIONS .EQ. GRPH_HARDCOPY ) THEN
         CALL PGEND
         ISTAT = PGBEG( 0, GRAPHICS_DEVICE_NAME, 1, 1 )
         IF ( ISTAT .NE. 1 ) STATUS = ISTAT
      END IF

*  Annul any errors from PGPLOT.
  999 CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

      CONTINUE

      END


      SUBROUTINE ECH_GR_SET_COLOUR( COLOUR )

      IMPLICIT NONE

      INTEGER COLOUR

      CALL PGSCI( COLOUR )

      END

      SUBROUTINE ECH_GR_SET_LINESTYLE( STYLE )

      IMPLICIT NONE

      INTEGER STYLE

      CALL PGSLS( STYLE )

      END
