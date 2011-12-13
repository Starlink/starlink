      INTEGER FUNCTION CHR_SIZE( STRING )
*+
*  Name:
*     CHR_SIZE

*  Purpose:
*     Return the declared size of a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_SIZE( STRING )

*  Description:
*     Give the declared size of a Fortran 77 character string variable,
*     including trailing blanks.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The character string of whose length is determined.

*  Returned Value:
*     CHR_SIZE = INTEGER
*        Returns the declared size of the string.

*  Algorithm:
*     Use the Fortran 77 LEN Function.

*  Notes:
*     This routine is OBSOLETE.  It exists for historical reasons.
*     Its function is identical to the Fortran 77 intrinsic function
*     LEN. It is recommended that the intrinsic function LEN be called
*     directly.

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
*     12-OCT-1984 (ACD):
*        Improve the documentation.
*     2-SEP-1988 (AJC):
*        Improve documentation.
*     30-MAR-1994 (ACC):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STRING * ( * )

*.

      CHR_SIZE = LEN( STRING )

      END
