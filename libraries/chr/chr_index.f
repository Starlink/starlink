      INTEGER FUNCTION CHR_INDEX( STRING, SUBSTR )
*+
*  Name:
*     CHR_INDEX

*  Purpose:
*     Return the index of a substring in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_INDEX( STRING, SUBSTR )

*  Description:
*     Find the position of a substring within a given string. If no
*     substring is found, the value zero is returned.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be searched.
*     SUBSTR = CHARACTER * ( * ) (Given)
*        The substring to be used in the search.

*  Returned Value:
*     CHR_INDEX = INTEGER
*        The position of SUBSTR within STRING.

*  Algorithm:
*     Use the Fortran 77 INDEX function.

*  Notes:
*     This routine is OBSOLETE.  It exists for historical reasons.
*     Its function is identical to the Fortran intrinsic function INDEX.
*     It is recommended that the INDEX intrinsic function be called
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
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-NOV-1984 (ACD):
*        Improved documentation.
*     2-SEP-1988 (AJC):
*        Added to description.
*     30-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STRING * ( * )
      CHARACTER SUBSTR * ( * )

*.

*  Use the Fortran INDEX intrinsic function to set the returned value.
      CHR_INDEX = INDEX( STRING, SUBSTR )

      END
