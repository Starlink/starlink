      INTEGER FUNCTION STR_LEN( STRING )
*+
*  Name:
*     LNBLNK

*  Purpose:
*     Return the length of a string, ignoring trailing blanks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = LNBLNK( STRING )

*  Description:
*     Find length of string, ignoring trailing blanks.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string whose length is to be determined.

*  Returned Value:
*     STR_LEN = INTEGER
*        Returns the used length of the string.

*  Algorithm:
*     Portable version:
*        Start from string's declared size and work back to start
*        until first non-blank character is found.
*  Authors:
*
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     BLY: M.J.Bly (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     2-OCT-1984 (ACD):
*        Documentation improved.
*     13-SEP-1988 (AJC):
*        Add VAX VMS version in comments (slower unless the tail is
*        very long).
*     3-OCT-1988 (AJC):
*        Improve documentation.
*     26-JAN-1990 (AJC):
*        Improve Method comments.
*     07-FEB-1997 (BLY):
*        This version borrwoed form CHR routine CHR_LEN for Linux port
*           of ECHWIND to provide LNBLNK Fortran Intrinsic not
*           available on Linux.
*     21-AUG-2005 (TIMJ):
*        It's more portable simply to use our own namespace rather than
*        guessing whether LNBLNK will exist on the particular compiler.
*        <sarcasm>Here's a thought: simply use CHR_LEN.
*        No, that would never work!</sarcasm>
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Portable version.
*  Local Variables:
      INTEGER TOTLEN             ! Declared length of the string
      INTEGER IPOSN              ! Current position in the string

*.

*  Get the declared length of the string.
      TOTLEN = LEN( STRING )

*  Loop to find the position of the last non-blank character.
      DO 10 IPOSN = TOTLEN, 1, -1
         IF ( STRING( IPOSN : IPOSN ) .NE. ' ' ) GO TO 20
 10   CONTINUE
 20   CONTINUE

      STR_LEN = IPOSN

      END
