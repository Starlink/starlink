      SUBROUTINE CHR_DELIM( STRING, DELIM, INDEX1, INDEX2 )
*+
*  Name:
*     CHR_DELIM

*  Purpose:
*     Locate a substring using a given delimiter character.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_DELIM( STRING, DELIM, INDEX1, INDEX2 )

*  Description:
*     The given character string is examined to see if it contains a
*     substring delimited by the character, DELIM. The indices of the
*     first and last characters of the substring are returned as
*     INDEX1 and INDEX2 respectively. If no occurrence of the specified
*     delimiter is found, or if the only occurrence is the last
*     character of the string, then the indices are returned pointing
*     to the whole of the input string. If only one occurrence of the
*     delimiter is found and it is not the last character in the string,
*     INDEX1 will point to this position and INDEX2 will point to the
*     last character in the string. If there are more than two
*     of the occurrences of the delimiter character, INDEX1 will point
*     to the first occurrence and INDEX2 to the last occurrence.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The character string to be searched.
*     DELIM = CHARACTER * 1 (Given)
*        The substring delimiting character.
*     INDEX1 = INTEGER (Returned)
*        The position of the first occurrence of the delimiter, or the
*        first character in the string.
*     INDEX2 = INTEGER (Returned)
*        The position of the last occurrence of the delimiter, or the
*        last character in the string.

*  Algorithm:
*     Get the declared length of the string.
*     Find the index to the delimiter in the whole of the string.
*     If the delimiter character is not found or the delimiter character
*     is the last character of the string then
*        Set the first index to 1 and the second index to the
*        declared length of the string.
*     else
*        Set the first index to the delimiter index.
*        Set the current positon in the string to the first index.
*        Repeat:
*           Get index to the delimiter character in the substring
*           from the current position index plus one to the end of
*           the string.
*           If the delimiter index is not zero then
*              Set the second index to the current position plus the
*              delimiter index.
*           endif
*        until the current delimiter index is zero, or the current
*        position is equal to or exceeds the declared length of the string.
*        If no second occurrence was found then
*           The second index is set equal to the declared length of
*           the string.
*        else
*           The second index is set to the position of the last
*           occurrence.
*        endif
*     endif

*  Copyright:
*     Copyright (C) 1984, 1988, 1994 Science & Engineering Research Council.
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
*     ASOC5: Dave Baines (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUN-1984 (ASOC5):
*        Rather more Vax specific version.
*     1-SEP-1988 (AJC):
*        Use LEN instead of CHR_SIZE.
*        Remove INCLUDE 'SAE_PAR'.
*     3-OCT-1988 (AJC):
*        Improve documentation.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRING
      CHARACTER DELIM

*  Arguments Returned:
      INTEGER INDEX1
      INTEGER INDEX2

*  Local Variables:
      INTEGER CURPOS             ! Current position index into string
      INTEGER DELIND             ! Index to delimiter character
      INTEGER SIZE               ! Declared size of passed string

*.

*  Get declared size of passed string.
      SIZE = LEN( STRING )

*  Find index to delimiter character in whole of passed string.
      DELIND = INDEX( STRING, DELIM )

      IF ( ( DELIND .LE. 0 ) .OR. ( DELIND .EQ. SIZE ) ) THEN

*     Delimiter character not found or delimiter character is
*     last character in string. Set the first index to 1 and the
*     second index to the declared size of the string.
         INDEX1 = 1
         INDEX2 = SIZE
      ELSE

*     Set the first index to the delimiter index.
         INDEX1 = DELIND

*     Set the current position in the string to the first index.
         CURPOS = INDEX1

*     Repeat until no further occurences of delimiter are found or
*     the string finishes.
*     DO WHILE loop.
 10      CONTINUE
         IF ( ( DELIND .GT. 0 ) .AND. ( CURPOS .LT. SIZE ) ) THEN

*        Get index to delimiter character in substring
*        from character after current delimiter to end of string.
            DELIND = INDEX( STRING( CURPOS+1 : SIZE ), DELIM )

*        Check if a delimiter has been found.
            IF ( DELIND .GT. 0 ) THEN

*           Reset current position.
               CURPOS = CURPOS + DELIND
            END IF
         GO TO 10
         END IF

*     Set second index.
         IF ( CURPOS .EQ. INDEX1 ) THEN

*        No second occurrence found.
            INDEX2 = SIZE
         ELSE

*        Set second index to last occurrence position.
            INDEX2 = CURPOS
         END IF
      END IF

      END
