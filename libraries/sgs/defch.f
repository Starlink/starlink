      SUBROUTINE sgs_DEFCH (CHOSTR)
*+
*  Name:
*     DEFCH

*  Purpose:
*     Define the valid keys for choice input from the command terminal.
*     Convert the specified characters to upper case and store them in
*     COMMON.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     CHOSTR = CHAR (Given)
*         Character string containing valid keys

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     sgs_1UPCAS

*  Written To Common:
*     CHOIST      c        current valid choice characters
*     LCHOST      i        number of valid choice characters

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'


      CHARACTER*(*) CHOSTR



*   Save length of string
      LCHOST = MIN(LEN(CHOSTR),MAXCHO)

*   Copy character into common block and convert to upper case
      CALL sgs_1UPCAS(CHOSTR(:LCHOST),CHOIST)

      END
