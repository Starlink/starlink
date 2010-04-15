*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused CLIPTR, NIT, IERR, GEN_ILEN
*-----------------------------------------------------------------------

      SUBROUTINE INS_CLI_ITEM (ITEM)

*  Routine to insert the given item in the CLI line for the current
*  stack level, at the current pointer position

      IMPLICIT  NONE

*  Formal parameters

      CHARACTER ITEM*(*)

*  Include files

      INCLUDE 'CLI_STACK.INC'

*  The section of the string being used for input at this level is
*       CLILINE(ICLIST:ICLIFIN) where ICLIST  = GEN_ICHTOT (ISP-1) + 1
*                               and   ICLIFIN = GEN_ICHTOT (ISP)

*  Local variables

      INTEGER*4 LITEM
      INTEGER*4 ICLIST
      INTEGER*4 ICLIFIN
      INTEGER*4 IST, IFIN
      CHARACTER BLANK*1

*  Functions etc

      INTEGER*4 GEN_ICHTOT

      DATA      BLANK      /' '/

*  OK. So do it. Basically what we need to do is to find where in the CLILINE
*  the item just read ends, and splice in the new bit between there and the
*  rest of CLILINE. To conserve space with lots of symbol translations it would
*  be better to chuck out all of the line that has already been read at this
*  level too! Add a blank after the inserted text: it can't hurt and is
*  necessary when the routine is used during symbol translation.

      LITEM   = LEN (ITEM)
      ICLIST  = GEN_ICHTOT (ISP-1) + 1
      ICLIFIN = GEN_ICHTOT (ISP)

      IST     = ICLI (1,ISP)
      IFIN    = ICLI (2,ISP)

CD    Print *, 'Original CLILINE for this stack level:'
CD    Print *,  CLILINE(ICLIST:ICLIFIN)

*  Then make the substitution: Note that length of new string is
*  equal to length of original string, less the characters removed,
*  plus the length of the inserted string (including the blank)

      CLILINE =   CLILINE(:ICLIST-1)
     &          //ITEM
     &          //BLANK
     &          //CLILINE(ICLIST+IST-1:)

      ICLIFIN = ICLIFIN - (IST-1) + LITEM + 1

*  Update the stack information to reflect changes: note that
*  the string preceding the inserted string has now been lost.

      ICLI(1,ISP) = 1
      ICLI(2,ISP) = ICLIFIN + 1 - ICLIST

CD    Print *,'Updated CLILINE for this stack level:'
CD    Print *, CLILINE(ICLIST:ICLIFIN)

      RETURN
      END
