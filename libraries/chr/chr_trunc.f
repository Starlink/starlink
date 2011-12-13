      SUBROUTINE CHR_TRUNC( DELIM, STRING )
*+
*  Name:
*     CHR_TRUNC

*  Purpose:
*     Truncate a string at a given delimiter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_TRUNC( DELIM, STRING )

*  Description:
*     The given string is truncated at the first occurrence of the
*     given delimiter character. The delimiter character and all
*     subsequent characters are replaced by blanks. If no delimiter
*     character is found in the string, no truncation takes place.
*     This routine is effectively a combination of INDEX and CHR_TERM.

*  Arguments:
*     DELIM = CHARACTER * 1 (Given)
*        The truncation delimiter character.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string to be truncated. All characters from, and
*        including, the first occurrence of DELIM will be
*        replaced with blanks.

*  Algorithm:
*     Attempt to find the first occurrence of the delimiter in the
*     string.
*     If the delimter is found then
*       Replace the delimiter and all subsequent elements with blanks.
*     end if

*  Copyright:
*     Copyright (C) 1984, 1988 Science & Engineering Research Council.
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
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JUN-1984 (ACD):
*        Original version.
*     13-SEP-1988 (AJC):
*       Change unhelpful (wrong) notes to Method section of prologue.
*       Remove calculation of LEN( STRING ).
*     3-OCT-1988 (AJC):
*        Improve documentation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER DELIM

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

*  Local Variables:
      INTEGER IPOSN              ! Position of the first occurrence of DELIM

*.

*  Try to find a delimiter character within the given string.
      IPOSN = INDEX( STRING, DELIM )

      IF ( IPOSN .GT. 0 ) THEN
         STRING( IPOSN : ) = ' '
      END IF

      END
