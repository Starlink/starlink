      SUBROUTINE sgs_DEFCH (CHOSTR)
*+
*   - - - - - -
*    D E F C H
*   - - - - - -
*
*   Define the valid keys for choice input from the command terminal.
*   Convert the specified characters to upper case and store them in
*   COMMON.
*
*   Given:
*      CHOSTR      c*(*)    character string containing valid keys
*
*   Written to COMMON:
*      CHOIST      c        current valid choice characters
*      LCHOST      i        number of valid choice characters
*
*   Externals:
*      sgs_1UPCAS
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      INCLUDE 'sgscom'


      CHARACTER*(*) CHOSTR



*   Save length of string
      LCHOST = MIN(LEN(CHOSTR),MAXCHO)

*   Copy character into common block and convert to upper case
      CALL sgs_1UPCAS(CHOSTR(:LCHOST),CHOIST)

      END
