      SUBROUTINE CHR_LTOC( LVALUE, STRING, NCHAR )
*+
*  Name:
*     CHR_LTOC

*  Purpose:
*     Encode a LOGICAL value as a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_LTOC( LVALUE, STRING, NCHAR )

*  Description:
*     Encode the given LOGICAL value as one of the character strings
*     'TRUE' or 'FALSE'.

*  Arguments:
*     LVALUE = LOGICAL (Given)
*        The value to be encoded.
*     STRING = CHARACTER * ( * ) (Returned)
*        The string into which the value is to be encoded.
*     NCHAR = INTEGER (Returned)
*        The field width used in encoding the value.

*  Algorithm:
*     If the given value is .TRUE., then
*       Encode 'TRUE' into the returned string.
*     else the given value is .FALSE.
*       Encode 'FALSE' into the returned string.
*     end if
*     Set the length of the returned string.

*  Copyright:
*     Copyright (C) 1983, 1984, 1989, 1990 Science & Engineering Research Council.
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     DLT: D.L. Terrett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     4-OCT-1984 (ACD):
*        Documentation improved.
*     16-JUN-1989 (AJC):
*        Avoid using CHR_LEN.
*     15-JAN-1990 (DLT):
*        Eliminate redundant EXTERNAL.
*     22-JAN-1997 (AJC):
*        NCHAR is known - don't need CHR_LEN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      LOGICAL LVALUE

*  Arguments Returned:
      CHARACTER STRING * ( * )

      INTEGER NCHAR
*.

*  Assign the value of the returned string.
      IF ( LVALUE ) THEN
         STRING = 'TRUE'
         NCHAR = 4
      ELSE
         STRING = 'FALSE'
         NCHAR = 5
      END IF

      END
