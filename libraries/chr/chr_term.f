      SUBROUTINE CHR_TERM( LENGTH, STRING )
*+
*  Name:
*     CHR_TERM

*  Purpose:
*     Terminate a string by padding out with blanks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_TERM( LENGTH, STRING )

*  Description:
*     The given string, STRING, is terminated to a length of LENGTH
*     characters by filling the remainder of its declared length
*     with blanks.

*  Arguments:
*     LENGTH = INTEGER (Given)
*        The required length for the string: it must be positive
*        and not greater than the declared length of the string.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string to be terminated.

*  Algorithm:
*     If the required length is not negative then
*        Get declared length of the given string.
*        If the required length is not longer than or equal to the
*        declared length then
*           Pad to the end of the string with blanks.
*        endif
*     endif

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1990 Science & Engineering Research Council.
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
*     ASOC5: Dave Baines (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     DLT: D.L. Terrett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     28-JUN-1984 (ASOC5):
*        Documentation increased.
*     2-SEP-1988 (AJC):
*        Replace CHR_SIZE with LEN.
*        Improve the Description.
*     25-JAN-1990 (DLT):
*        Eliminate redundant EXTERNAL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER LENGTH

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

*.

*  Check that required length is not negative.
      IF( LENGTH .GE. 0 ) THEN

*     Check that the required length is not greater than or equal to
*     declared length.
         IF ( LENGTH .LT. LEN( STRING ) ) THEN

*        Pad to the end of the string with blanks.
            STRING( LENGTH+1 : ) = ' '
         END IF
      END IF

      END
