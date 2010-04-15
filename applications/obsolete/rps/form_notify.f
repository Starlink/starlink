*+FORM_NOTIFY      Puts up screen (pop_menu) with message
*     1993 June         P. Brisco       Got rid of SMG stuff.
*---------------------------------------------------------------------------
      SUBROUTINE FORM_NOTIFY(HEADER,MESSAGE)
      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*(*) HEADER		! will only take 30 chars
      CHARACTER*(*) MESSAGE		! up to 4 * 64 chars

*  Functions
      INTEGER POP_MENU

*  Local Variables
      CHARACTER*30 NOTHEAD
      CHARACTER*17 OPT/'<ret> to continue'/
      INTEGER STATUS

*  Executable Code

      NOTHEAD = HEADER
      STATUS = POP_MENU(OPT,1,NOTHEAD,1, MESSAGE)

      END
