      SUBROUTINE CHR_RMBLK( STRING )
*+
*  Name:
*     CHR_RMBLK

*  Purpose:
*     Remove all blanks from a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_RMBLK( STRING )

*  Description:
*     All leading and embedded blanks in the string are removed.
*     The remaining characters are moved to the left to eliminate the
*     resulting empty space, and the end of the string is filled with
*     blanks.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string from which all leading and embedded blanks are
*        removed.

*  Algorithm:
*     Work from the front of the string, copying the non-blank
*     characters into the string itself.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1991 Science & Engineering Research Council.
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
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-OCT-1984 (ACD):
*        Documentation improved.
*     3-OCT-1988 (AJC):
*        Documentation improved.
*     20-FEB-1991 (PCTR):
*        Replaced call to CHR_TERM with in-line equivalent.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER J                  ! Loop index
      INTEGER SIZE               ! Character count

*.

*  Get the length of the string.
      SIZE = CHR_LEN( STRING )

*  Initialise count of characters copied.
      J = 0

*  Loop to perform the removal of blank space.
      DO 10 I = 1, SIZE

         IF ( STRING( I : I ) .NE. ' ' ) THEN
            J = J + 1
            STRING( J : J ) = STRING( I : I )
         END IF
 10   CONTINUE

*  Remove any duplicate characters remaining at the end of the string.
      IF ( J .LT. SIZE ) STRING( J+1 : ) = ' '

      END
