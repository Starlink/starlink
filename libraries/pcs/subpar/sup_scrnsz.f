      SUBROUTINE SUBPAR_SCRNSZ (SCREEN_WIDTH, SCREEN_HEIGHT, STATUS)

*    Description :
*     This is the Unix version
*     It interrogates the system to find the width and height
*     of the screen on which it is running.

*    Invocation :
*     CALL SUBPAR_SCRNSZ (SCREEN_WIDTH, SCREEN_HEIGHT, STATUS)

*    Parameters :
*     SCREEN_WIDTH   =INTEGER       (returned)
*            width of the screen - defaults to 80 characters
*     SCREEN_HEIGHT  =INTEGER       (returned)
*            height of the screen - defaults to 0 lines,
*            ie no paging (eg if running in batch)

*    Method :
*     Call the C function SUP_TRMSZ.
*     If this is unsuccessful, set the default which will result in no
*     paging.

*    Deficiencies :
*     <description of any deficiencies>

*    Bugs :
*     <description of any "bugs" which have not been fixed>

*    Authors :
*     A. J. Chipperfield  (RLVAD::AJC)

*    History :
*     17.06.92:  Original
*    endhistory

*    Type Definitions :
      IMPLICIT NONE

*    Global constants
      INCLUDE 'SAE_PAR'             ! SAE constants

*    Export :
      INTEGER SCREEN_WIDTH          ! Screen width (characters)
      INTEGER SCREEN_HEIGHT         ! Screen height (lines)

*    Status :
      INTEGER STATUS                ! Global status

*    External References :
      INTEGER SUBPAR_TRMSZ
      EXTERNAL SUBPAR_TRMSZ

*    Local variables :
      INTEGER ISTAT                 ! Local status

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Inquire the terminal size
      ISTAT = SUBPAR_TRMSZ ( SCREEN_WIDTH, SCREEN_HEIGHT )

*   If this failed to get a good value, set default which causes no paging.
      IF ( ( ISTAT .NE. 1 ) .OR. ( SCREEN_WIDTH .LE. 0 ) ) THEN
         SCREEN_WIDTH = 80
         SCREEN_HEIGHT = 0
      ENDIF

      END

