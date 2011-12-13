      SUBROUTINE CHR_TOCHR( CHARS, STRING, FORWD, IPOSN )
*+
*  Name:
*     CHR_TOCHR

*  Purpose:
*     Skip to the next specified character in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_TOCHR( CHARS, STRING, FORWD, IPOSN )

*  Description:
*     Increment a character pointer, IPOSN, either forward or
*     backward through a string, until the character pointed to
*     is one of a specified set of characters. The direction of
*     the search is given by the argument FORWD. If no such
*     character position exists (i.e. none of the remaining
*     characters in the string are members of the specified set),
*     the pointer is returned set to one more than the length of
*     the string if the search is in the forward direction, or
*     zero if the search is in the reverse direction. If the initial
*     value of IPOSN does not point at one of the characters in the
*     string, then the routine will return without action.

*  Arguments:
*     CHARS = CHARACTER * ( * ) (Given)
*        A string consisting of the set of characters to be searched
*        for.
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be searched.
*     FORWD = LOGICAL (Given)
*        The search direction: if .TRUE. then proceed through the string
*        in a forward direction, otherwise work backwards through the
*        string.
*     IPOSN = INTEGER (Given and Returned)
*        The character pointer.

*  Copyright:
*     Copyright (C) 1990, 1991, 1994 Science & Engineering Research Council.
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
*     7-SEP-1990 (RFWS):
*        Original version.
*     20-FEB-1991 (PCTR):
*        Converted to CHR_ and added FORWD argument.
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
      CHARACTER * ( * ) STRING

      LOGICAL FORWD

*  Arguments Given and Returned:
      INTEGER IPOSN

*  Local Variables:
      INTEGER END                ! End loop index
      INTEGER II                 ! Loop counter for character positions
      INTEGER INCR               ! Loop increment: 1=forward, -1=reverse
      INTEGER START              ! Start loop index
      INTEGER STRLEN             ! Declared length of STRING

*.

*  Initialise STRLEN.
      STRLEN = LEN( STRING )

*  Check if the initial value of IPOSN is in range.
      IF ( ( IPOSN .GE. 1 ) .AND. ( IPOSN .LE. STRLEN ) ) THEN

*     If so, set increment.
         IF( FORWD ) THEN
            INCR = 1
            START = IPOSN
            END = STRLEN
         ELSE
            INCR = -1
            START = IPOSN
            END = 1
         END IF

*     Loop to inspect characters, stopping when one is found which is
*     not in CHARS.
         DO 10 II = START, END, INCR
            IF ( INDEX( CHARS, STRING( II : II ) ) .NE. 0 ) GO TO 20
 10      CONTINUE
 20      CONTINUE

*     Return the new pointer value.
         IPOSN = II
      END IF

      END
