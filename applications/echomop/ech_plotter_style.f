      SUBROUTINE ECH_PLOTTER_STYLE( STYLE, NEW_STYLE, STATUS )
*+
*-
      IMPLICIT NONE

      INCLUDE 'ECH_ENVIRONMENT.INC'

      CHARACTER*( * ) STYLE
      CHARACTER*( * ) NEW_STYLE
      INTEGER STATUS

      INTEGER MAX_FONTS
      PARAMETER ( MAX_FONTS = 4 )

      INTEGER MAX_COLOURS
      PARAMETER ( MAX_COLOURS = 8 )

      INTEGER MAX_LINESTYLES
      PARAMETER ( MAX_LINESTYLES = 5 )

      INTEGER IWIDTH
      INTEGER IDUM
      INTEGER I

      CHARACTER*128 KEYS

      INTEGER CHR_LEN

*  Data Statements:
      DATA IWIDTH / 1 /
      INCLUDE 'ECH_PLOT_PARS.INC'
*.

      IF ( NEW_STYLE .EQ. 'BINS' .OR.
     :     NEW_STYLE .EQ. 'LINES' .OR.
     :     NEW_STYLE .EQ. 'POINTS' .OR.
     :     NEW_STYLE .EQ. '+' .OR.
     :     NEW_STYLE .EQ. '*' .OR.
     :     NEW_STYLE .EQ. '#' .OR.
     :     NEW_STYLE .EQ. 'X' ) THEN
         STYLE = NEW_STYLE
         GO TO 999

      ELSE IF ( NEW_STYLE .EQ. 'THICKER' ) THEN
         IWIDTH = MIN( 201, IWIDTH + 1 )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, FLOAT( IWIDTH ), 0., 0.,
     :        0., ' ', ' ', ' ', 0., 0., GRPH_SET_WIDTH, ' ', STATUS )
         GO TO 999

      ELSE IF ( NEW_STYLE .EQ. 'THINNER' ) THEN
         IWIDTH = MAX( 1, IWIDTH - 1 )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, FLOAT( IWIDTH ), 0., 0.,
     :        0., ' ', ' ', ' ', 0., 0., GRPH_SET_WIDTH, ' ', STATUS )
         GO TO 999

      ELSE
         DO I = 1, MAX_COLOURS
            IF ( COLOURS( I ) .EQ. NEW_STYLE ) THEN
               CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 0., 0., 0., 0.,
     :              NEW_STYLE, ' ', ' ', 0., 0.,
     :              GRPH_SET_COLOUR, ' ', STATUS )
               GO TO 999
            END IF
         END DO
         DO I = 1, MAX_LINESTYLES
            IF ( LINE_STYLES( I ) .EQ. NEW_STYLE ) THEN
                CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 0., 0., 0., 0.,
     :               ' ', NEW_STYLE, ' ', 0., 0.,
     :               GRPH_SET_LINE_STYLE, ' ', STATUS )
               GO TO 999
            END IF
         END DO
         DO I = 1, MAX_FONTS
            IF ( FONTS( I ) .EQ. NEW_STYLE ) THEN
               CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 0., 0., 0., 0.,
     :              NEW_STYLE, ' ', ' ', 0., 0., GRPH_SET_FONT, ' ',
     :              STATUS )
               GO TO 999
            END IF
         END DO
      END IF

      KEYS = ' Unknown style-control word: ' //
     :      NEW_STYLE( :CHR_LEN( NEW_STYLE ) ) // '.'
      CALL ECH_REPORT( 0, KEYS )
      CALL ECH_REPORT( 0,
     :     ' Recognised words are:' )
      CALL ECH_REPORT( 0,
     :     '    Graph types  - BINS, LINES, POINTS, +, *, #, X' )
      KEYS = '    Plot colours - ' //
     :      COLOURS( 1 )( :CHR_LEN( COLOURS( 1 ) ) )
      DO I = 2, MAX_COLOURS
         KEYS = KEYS( :CHR_LEN( KEYS ) ) // ', ' //
     :         COLOURS( I )( :CHR_LEN( COLOURS( I ) ) )
      END DO
      CALL ECH_REPORT( 0, KEYS )
      KEYS = '    Line styles  - ' //
     :      LINE_STYLES( 1 )( :CHR_LEN( LINE_STYLES( 1 ) ) )
      DO I = 2, MAX_LINESTYLES
         KEYS = KEYS( :CHR_LEN( KEYS ) ) // ', ' //
     :         LINE_STYLES( I )( :CHR_LEN( LINE_STYLES( I ) ) )
      END DO
      CALL ECH_REPORT( 0, KEYS )
      KEYS = '    Font styles  - ' //
     :      FONTS( 1 )( :CHR_LEN( FONTS( 1 ) ) )
      DO I = 2, MAX_FONTS
         KEYS = KEYS( :CHR_LEN( KEYS ) ) // ', ' //
     :         FONTS( I )( :CHR_LEN( FONTS( I ) ) )
      END DO
      CALL ECH_REPORT( 0, KEYS )
      CALL ECH_REPORT( 0, ' ' )

  999 CONTINUE

      END
