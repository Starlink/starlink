      SUBROUTINE ECH_READ_GRPH_CURSOR( STATUS )
*+
*  Name:
*     ECHOMOP - ECH_READ_GRPH_CURSOR

*  Purpose:
*     Read graphics cursor.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL OLD_X_CURSOR
      REAL OLD_Y_CURSOR

      CHARACTER*1 TEMP_CHAR

      COMMON / SAVE_CURSE / OLD_X_CURSOR, OLD_Y_CURSOR
*.
      STATUS = 0
      IF ( .NOT. GRAPHICS_SETUP ) CALL ECH_SETUP_GRAPHICS( STATUS )

      IF ( STATUS .EQ. 0 .AND. GRAPHICS_SETUP .AND. IPGCUR .EQ. 1 ) THEN
         TEMP_CHAR = '`'
         X_CURSOR = OLD_X_CURSOR
         Y_CURSOR = OLD_Y_CURSOR
         CALL PGCURSE( X_CURSOR, Y_CURSOR, TEMP_CHAR )
         OLD_X_CURSOR = X_CURSOR
         OLD_Y_CURSOR = Y_CURSOR
         IF ( TEMP_CHAR .NE. '`' .AND. TEMP_CHAR .NE. '1' .AND.
     :        TEMP_CHAR .NE. '2' .AND. TEMP_CHAR .NE. '4' ) THEN
           USER_INPUT_CHAR = TEMP_CHAR
           CALL CHR_UCASE( USER_INPUT_CHAR )

         ELSE
           CALL ECH_REPORT( 0,
     :        ' Keyboard not linked to graphics cursor.' )
           CALL ECH_REPORT( 0,
     :        ' Please press required option key now...' )
           CALL ECH_READ_KEYBOARD( STATUS )
           CALL CHR_UCASE( USER_INPUT_CHAR )
         END IF

      ELSE
         CALL ECH_REPORT( 0,
     :        ' No graphics cursor available: use M to set position.' )
         CALL ECH_REPORT( 0,
     :        ' Please press required option key now...' )
         CALL ECH_READ_KEYBOARD( STATUS )
      END IF

      END
