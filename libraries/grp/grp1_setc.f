      SUBROUTINE GRP1_SETC( FIRST, LAST, SIZE, ARRAY, VALUE, STATUS )
*+
*  Name:
*     GRP1_SETC

*  Purpose:
*     Set part of a character array to a constant value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_SETC( FIRST, LAST, SIZE, ARRAY, VALUE, STATUS )

*  Description:
*     The given value is written into the given part of the array.

*  Arguments:
*     FIRST = INTEGER (Given)
*        The first array element to be written to.
*     LAST = INTEGER (Given)
*        The last array element to be written to.
*     SIZE = INTEGER (Given)
*        The total size of the array.
*     ARRAY( SIZE ) = CHARACTER * ( * ) (Given and Returned)
*        The array.
*     VALUE = CHARACTER * ( * ) (Given)
*        The value to write to the array.
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
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FIRST
      INTEGER LAST
      CHARACTER VALUE*(*)
      INTEGER SIZE

*  Arguments Given and Returned:
      CHARACTER ARRAY( SIZE )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the given value to the given range of the array.
      DO I = MAX( 1, FIRST ), MIN( SIZE, LAST )
         ARRAY( I ) = VALUE
      END DO

      END
