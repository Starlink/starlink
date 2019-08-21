      INTEGER FUNCTION NDF1_LEN( STR )
*+
*  Name:
*     NDF1_LEN

*  Purpose:
*     Return the declared length of a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = NDF1_LEN( STR )

*  Description:
*     The routine returns the number of characters in the string
*     supplied, as determined by the Fortran intrinsic LEN function.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The string.

*  Returned Value:
*     NDF1_LEN = INTEGER
*        The string's length.

*  Notes:
*     This routine exists purely to avoid using the intrinsic LEN
*     function in generic routines, where the compiler might otherwise
*     object to its argument having an incorrect data type (even though
*     such calls would never actually be executed).

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     29-JUL-1994 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR

*.

*  Return the string length.
      NDF1_LEN = LEN( STR )

      END
