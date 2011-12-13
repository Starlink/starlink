      SUBROUTINE CHR_PUTL( LVALUE, STRING, IPOSN )
*+
*  Name:
*     CHR_PUTL

*  Purpose:
*     Put a LOGICAL value into a string at a given position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_PUTL( LVALUE, STRING, IPOSN )

*  Description:
*     The LOGICAL value is encoded into 'T' or 'F' which is then
*     copied into the given string beginning at position IPOSN+1.
*     IPOSN is is returned updated to indicate the end position
*     of the encoded logical value within STRING.

*  Arguments:
*     LVALUE = LOGICAL (Given)
*        The LOGICAL value to be encoded into the string.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string into which LVALUE is to be copied.
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
*     3-OCT-1988 (AJC):
*        Documentation improved.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      LOGICAL LVALUE

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

      INTEGER IPOSN

*.

*  Use CHR_PUTC to encode 'T' or 'F' into the given string.
      IF ( LVALUE ) THEN
         CALL CHR_PUTC( 'T', STRING, IPOSN )
      ELSE
         CALL CHR_PUTC( 'F', STRING, IPOSN )
      END IF

      END
