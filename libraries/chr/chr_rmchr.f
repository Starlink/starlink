      SUBROUTINE CHR_RMCHR( CHARS, STRING )
*+
*  Name:
*     CHR_RMCHR

*  Purpose:
*     Remove all specified characters from a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_RMCHR( CHARS, STRING )

*  Description:
*     Remove a specified set of characters from a string in situ.
*     The remaining characters are moved to the left to eliminate
*     the resulting empty space, and the end of the string is filled
*     with blanks.

*  Arguments:
*     CHARS = CHARACTER * ( * ) (Given)
*        A string specifying all the characters which are to be removed.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string from which the characters are removed.

*  Copyright:
*     Copyright (C) 1990, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-SEP-1990 (RFWS):
*        Original version.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) CHARS

*  Arguments Given and Returned:
      CHARACTER * ( * ) STRING

*  Local Variables:
      INTEGER I                  ! Loop counter for input characters
      INTEGER J                  ! Number of output characters
      INTEGER SIZE               ! Declared length of STRING

*.

*  Get size of string.
      SIZE = LEN( STRING )

*  Initialise count of characters remaining.
      J = 0

*  Loop to test each character.
      DO 10 I = 1, SIZE

*  If it is not to be removed, then copy it to its new location in the
*  string.
         IF ( INDEX( CHARS, STRING ( I : I ) ) .EQ. 0 ) THEN
            J = J + 1
            STRING( J : J ) = STRING ( I : I )
         END IF
 10   CONTINUE

*  Fill extra spaces on the right with spaces.
      IF ( J .LT. SIZE ) STRING( J+1 : ) = ' '

      END
