      SUBROUTINE CHR_PUTI( IVALUE, STRING, IPOSN )
*+
*  Name:
*     CHR_PUTI

*  Purpose:
*     Put an INTEGER value into a string at a given position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_PUTI( IVALUE, STRING, IPOSN )

*  Description:
*     The INTEGER value is encoded into a concise string which is
*     then copied into the given string beginning at position IPOSN+1.
*     IPOSN is returned updated to indicate the end position of the
*     encoded number within STRING. This is a combination of CHR_ITOC
*     and CHR_PUTC.

*  Arguments:
*     IVALUE = INTEGER (Given)
*        The value to be encoded into the string.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string into which IVALUE is to be copied.
*     IPOSN = INTEGER (Given and Returned)
*        The position pointer within STRING.

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
*     2-OCT-1984 (ACD):
*        Documentation improved.
*     13-SEP-1988 (AJC):
*        Remove calculation of LEN( STRING ).
*        Documentation improved.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER IVALUE

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

      INTEGER IPOSN

*  Local Constants:
      INTEGER SZSTR              ! Token size
      PARAMETER ( SZSTR = 80 )

*  Local Variables:
      CHARACTER STR1 * ( SZSTR ) ! Temporary string to hold number

      INTEGER SIZE1              ! Size of STR1

*.

*  Perform copy using calls to CHR_ITOC and CHR_PUTC.
      CALL CHR_ITOC( IVALUE, STR1, SIZE1 )
      CALL CHR_PUTC( STR1( 1 : SIZE1 ), STRING, IPOSN )

      END
