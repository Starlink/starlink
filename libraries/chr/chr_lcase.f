      SUBROUTINE CHR_LCASE( STRING )
*+
*  Name:
*     CHR_LCASE

*  Purpose:
*     Convert a string to lowercase.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_LCASE( STRING )

*  Description:
*     The characters in the string are all converted to lowercase
*     in situ.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string to be converted to lowercase.

*  Algorithm:
*     Use CHR_LEN to find the string length, and then CHR_LOWER to
*     convert each character.

*  Implementation Deficiencies:
*     This implementation does not use VAX utilities and hence
*     is slower than it could be. However, it retains the advantage
*     of being standard Fortran 77.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1994 Science & Engineering Research Council.
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
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-NOV-1984 (ACD):
*        Documentation improved.
*     3-OCT-1988 (AJC):
*        Documentation improved.
*     10-MAR-1994 (ACC):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

*  External References:
      CHARACTER * 1 CHR_LOWER    ! Convert character to lowercase

      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      INTEGER IPOSN              ! Current position in the string
      INTEGER SIZE               ! Length of the string

*.

*  Get the length of the given string, ignoring trailing blanks.
      SIZE = CHR_LEN( STRING )

*  Loop to convert the string to lowercase.
      DO 10 IPOSN = 1, SIZE
         STRING( IPOSN : IPOSN ) = CHR_LOWER( STRING( IPOSN : IPOSN ) )
 10   CONTINUE

      END
