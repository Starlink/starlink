*+  AIO_REWRITE - Write text to an output channel with cursor control
      SUBROUTINE AIO_REWRITE( ID, MODE, TEXT, STATUS )
*
*    Description :
*
*     Writes text to output channel with various kinds of cursor control.
*     These are controlled by the MODE argument, which can have the
*     following values,
*
*       NOCR	- Don't perform carriage-control or line-feed after
*                 printing string
*       TERM    - Print string from current position as normal. Useful
*                 for terminating a re-write sequence
*       BACKSP  - Backspace to restore cursor to same position as it
*                 was before the string was printed
*
*     Only the first character of the MODE is significant. AIO_REWRITE
*     obly works to the console device.
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*      8 Jun 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'AIO_PAR'
      INCLUDE 'PAR_PAR'
*
*    Global variables :
*
      INCLUDE 'ASTLIB(AIO_CMN)'
*
*    Import :
*
      INTEGER			ID			! Channel id
      CHARACTER*(*)		MODE			! Output mode
      CHARACTER*(*)		TEXT			! Text to output
*
*    Status :
*
      INTEGER 			STATUS
*
*    External references :
*
      EXTERNAL                  AIO_BLK
*
*    Local variables :
*
      CHARACTER*200		BUF			! Output buffer
      CHARACTER*1		C1			! First char of MODE

      INTEGER			BLEN			! Buffer length
      INTEGER                   LID                     ! Local channel id
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    If AIO isn't up and running, write to terminal
      IF ( .NOT. AIO_DEF ) THEN
        LID = AIO__M_CONSOLE
      ELSE
        LID = ID
      END IF

*    Only output to console
      IF ( LID .EQ. AIO__M_CONSOLE ) THEN

*      Translate any tokens
        IF ( INDEX(TEXT,'^') .GT. 0 ) THEN
          CALL MSG_LOAD( ' ', TEXT, BUF, BLEN, STATUS )
        ELSE
          BUF = TEXT
          BLEN = LEN(TEXT)
        END IF

*      Extract and capitalise first character
        C1 = MODE(1:1)
        CALL CHR_UCASE( C1 )

*      Switch on mode. Backspace mode?
        IF ( C1 .EQ. 'B' ) THEN

          CALL AIO1_REWRITE( C1, BUF(:BLEN), STATUS )

*      No carriage-control?
        ELSE IF ( C1 .EQ. 'N' ) THEN

          CALL AIO1_REWRITE( C1, BUF(:BLEN), STATUS )

*      Normal termination
        ELSE IF ( C1 .EQ. 'T' ) THEN

          CALL AIO1_REWRITE( C1, BUF(:BLEN), STATUS )

*      Otherwise invalid mode
        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'MODE', MODE )
          CALL ERR_REP( ' ', 'Unrecognised re-write mode /^MODE/',
     :                  STATUS )

        END IF

      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_REWRITE', STATUS )
      END IF

      END
