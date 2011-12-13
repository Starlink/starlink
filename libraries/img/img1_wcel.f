      SUBROUTINE IMG1_WCEL( SIZE, N, VALUE, ARRAY, STATUS )
*+
* Name:
*    IMG1_WCEL

*  Purpose:
*     Writes to the nth element of a 1-d character array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_WCEL( SIZE, N, VALUE, ARRAY, STATUS )

*  Description:
*     This routine writes to the Nth element of the given character
*     array. It is most useful when dealing with mapped arrays,
*     otherwise direct assignment should be used.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the array of characters.
*     N = INTEGER (Given)
*        The index of the element to be written to.
*     VALUE = CHARACTER * ( * ) (Given)
*        The string to be copied into the character array at position N.
*     ARRAY( SIZE ) = CHARACTER * ( * ) (Given and Returned)
*        The array of character strings.
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
*     6-SEP-1994 (PDRAPER):
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
      CHARACTER * ( * ) VALUE

*  Arguments Given and Returned:
      CHARACTER * ( * ) ARRAY( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*.
      IF ( STATUS .NE. SAI__OK ) RETURN
      ARRAY( N ) = VALUE
      END
* $Id$
