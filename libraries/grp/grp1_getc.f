      SUBROUTINE GRP1_GETC( SIZE, ARRAY, INDEX, TEXT, STATUS )
*+
*  Name:
*     GRP1_GETC

*  Purpose:
*     Get an element from a character array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_GETC( SIZE, ARRAY, INDEX, TEXT, STATUS )

*  Description:
*     The string held in the given array at the given index is returned.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the array specified by argument ARRAY.
*     ARRAY( SIZE ) = CHARACTER * ( * ) (Given)
*        The array.
*     INDEX = INTEGER (Given)
*        The index of the element to get from ARRAY.
*     TEXT = CHARACTER * ( * ) (Returned)
*        The text from the specified element of ARRAY.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Arguments Given:
      INTEGER SIZE
      CHARACTER ARRAY( SIZE )*(*)
      INTEGER INDEX

*  Arguments Returned:
      CHARACTER TEXT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the supplied index is outside the array bounds, return a blank
*  string.
      IF( INDEX .LE. 0 .OR. INDEX .GT. SIZE ) THEN
         TEXT = ' '

*  Otherwise, get the array element.
      ELSE
         TEXT = ARRAY( INDEX )

      END IF

      END
