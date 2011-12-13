      SUBROUTINE CHR_SWAP( CHAR1, CHAR2 )
*+
*  Name:
*     CHR_SWAP

*  Purpose:
*     Swap two single-character variables.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_SWAP( CHAR1, CHAR2 )

*  Description:
*     Exchange the values of two single-character variables.

*  Arguments:
*     CHAR1 = CHARACTER * 1 (Given and Returned)
*        The first character.
*     CHAR2 = CHARACTER * 1 (Given and Returned)
*        The second character.

*  Algorithm:
*     Use a third (temporary) character variable during the swap.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-NOV-1984 (ACD):
*        Documentation improved.
*     3-OCT-1988 (AJC):
*        Documentation improved.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER * 1 CHAR1
      CHARACTER * 1 CHAR2

*  Local Variables:
      CHARACTER * 1 TEMP         ! Temporary copy

*.

*  Swap the two characters.
      TEMP = CHAR1
      CHAR1 = CHAR2
      CHAR2 = TEMP

      END
