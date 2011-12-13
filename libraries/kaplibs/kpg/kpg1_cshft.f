      SUBROUTINE KPG1_CSHFT( N, TEXT )
*+
*  Name:
*     KPG1_CSHFT

*  Purpose:
*     Shifts the characters left or right in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CSHFT( N, TEXT )

*  Description:
*     This routine shifts all characters in a string by a given number of
*     characters, padding with spaces at the ends.

*  Arguments:
*     N = INTEGER (Returned)
*        The number of characters to shift to the right. Negative values
*        produce shifts to the left.
*     TEXT = CHARACTER * ( * ) (Given and Returned)
*        The text to be shifted.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER N

*  Arguments Given and Returned:
      CHARACTER TEXT*(*)


*  Local Variables:
      CHARACTER C*1              ! Character being moved
      INTEGER F                  ! Index of first non-blank character
      INTEGER FIRST              ! Index of first character to be moved
      INTEGER I                  ! Index of character in supplied text
      INTEGER J                  ! Index of character in returned text
      INTEGER L                  ! Index of last non-blank character
      INTEGER LAST               ! Index of last character to be moved
*.

*  Find the indices of the first and last non-blank characters in the
*  supplied text.
      CALL CHR_FANDL( TEXT, F, L )

*  Do nothing if the string is all blank, or has zero length.
      IF( L .GT. 0 ) THEN

*  First deal with shifts to the right.
         IF( N .GT. 0 ) THEN

*  Find the index within the supplied text of the last non-blank character
*  in the returned text.
            LAST = MIN( LEN( TEXT ) - N, L )

*  Note the index at which this character will be stored in the returned
*  string.
            J = LAST + N

*  Work backwards through the characters in the supplied text starting at
*  this index. Stop at the first non-blank character.
            DO I = LAST, F, -1
               C = TEXT( I : I )

*  Store the character at its new position in the string, and decrement
*  the index of the next character to be stored in the returned string.
               TEXT( J : J ) = C
               J = J - 1
            END DO

*  Pad the start with spaces.
            TEXT( F : J ) = ' '

*  Now deal with shifts to the left.
         ELSE IF( N .LT. 0 ) THEN

*  Find the index within the supplied text of the first non-blank character
*  in the returned text.
            FIRST = MAX( 1 - N, F )

*  Note the index at which this character will be stored in the returned
*  string.
            J = FIRST + N

*  Work forwards through the characters in the supplied text starting at
*  this index. Stop at the last non-blank character.
            DO I = FIRST, L
               C = TEXT( I : I )

*  Store the character at its new position in the string, and increment
*  the index of the next character to be stored in the returned string.
               TEXT( J : J ) = C
               J = J + 1
            END DO

*  Pad the end with spaces.
            TEXT( J : L ) = ' '

         END IF

      ENDIF

      END
