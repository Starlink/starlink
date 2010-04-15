*+FORM_ERR         Puts up screen (pop_menu) with error message
*	DATE		AUTHOR			DESCRIPTION
*	???		RAL			original
*     1993 June         P. Brisco       Got rid of SMG stuff.
****************************************************************************
      SUBROUTINE FORM_ERR(MESSAGE)
      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*(*) MESSAGE		!

*  Functions
      INTEGER POP_MENU

*  Local Variables
      CHARACTER*20 ERRHEAD/'RPS - error detected'/
      CHARACTER*17 ERROPT/'<ret> to continue'/
      INTEGER STATUS

*  Executable Code

      STATUS = POP_MENU(ERROPT,1,ERRHEAD, -1, MESSAGE )

      END
