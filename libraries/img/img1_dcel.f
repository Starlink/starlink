      SUBROUTINE IMG1_DCEL( SIZE, N, ARRAY, STATUS )
*+
* Name:
*    IMG1_DCEL

*  Purpose:
*     Deletes the nth element of a 1-d character array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_DCEL( SIZE, N, ARRAY, STATUS )

*  Description:
*     This routine deletes the Nth element of the given character
*     array. It does this by adjusting the position of all the elements
*     after the one to be deleted.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the array of characters.
*     N = INTEGER (Given)
*        The index of the element to be removed.
*     ARRAY( SIZE ) = CHARACTER * ( * ) (Given and Returned)
*        The array of character strings. (Note the size of this array
*        will be effectively one less on exit).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     8-SEP-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER SIZE
      INTEGER N

*  Arguments Given and Returned:
      CHARACTER * ( * ) ARRAY( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over all the elements starting from the one after the element to
*  be deleted. Check that the value for N is valid and that adjustment
*  is really necessary (no work for last element).
      IF ( N .GT. 0 .AND. N .LT. SIZE ) THEN
         DO 1 I = N + 1, SIZE
            ARRAY( I - 1 ) = ARRAY( I )
 1       CONTINUE
      END IF
      END
* $Id$
