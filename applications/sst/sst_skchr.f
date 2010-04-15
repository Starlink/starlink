      SUBROUTINE SST_SKCHR( CHARS, STR, I )
*+
*  Name:
*     SST_SKCHR

*  Purpose:
*     Skip over a set of characters in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_SKCHR( CHARS, STR, I )

*  Description:
*     The routine increments a pointer to a character position in a
*     string until the character pointed at is not one of a specified
*     set of characters. If no such character position exists, the
*     pointer is set to one more then the length of the string.

*  Arguments:
*     CHARS = CHARACTER * ( * ) (Given)
*        A string consisting of the set of characters to be skipped
*        over.
*     STR = CHARACTER * ( * ) (Given)
*        The string to be searched.
*     I = INTEGER (Given and Returned)
*        The character pointer.

*  Notes:
*     If the initial value of I does not point at one of the characters
*     in the string, then the routine returns without action.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-SEP-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) CHARS
      CHARACTER * ( * ) STR

*  Arguments Given and Returned:
      INTEGER I

*  Local Variables:
      INTEGER II                 ! Loop counter for character positions

*.

*  Check if the initial value of I is in range.
      IF ( ( I .GE. 1 ) .AND. ( I .LE. LEN( STR ) ) ) THEN

*  If so, then loop to inspect characters, stopping when one is found
*  which is not in CHARS.
         DO 1 II = I, LEN( STR )
            IF ( INDEX( CHARS, STR( II : II ) ) .EQ. 0 ) GO TO 2
1        CONTINUE
2        CONTINUE

*  Return the new pointer value.
         I = II
      END IF

      END
* @(#)sst_skchr.f   1.1   94/12/05 11:31:34   96/07/05 10:27:30
