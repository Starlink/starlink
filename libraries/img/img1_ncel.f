      CHARACTER * ( * ) FUNCTION IMG1_NCEL( ARRAY, SIZE, N, STATUS )
*+
* Name:
*    IMG1_NCEL

*  Purpose:
*     Returns the nth element of a 1-d character array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = IMG1_NCEL( ARRAY, SIZE, N, STATUS )

*  Description:
*     This routine returns the value of the Nth element of the given
*     character array. It is most useful when dealing with mapped
*     arrays, otherwise direct assignment should be used.

*  Arguments:
*     ARRAY( SIZE ) = CHARACTER * ( * ) (Given)
*        The array of character strings.
*     SIZE = INTEGER (Given)
*        The size of the array of characters.
*     N = INTEGER (Given)
*        The index of the required element.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     IMG1_NCEL = CHARACTER * ( * )
*        The value of the Nth element.

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
*     2-SEP-1994 (PDRAPER):
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
      CHARACTER * ( * ) ARRAY( SIZE )
      INTEGER N

*  Status:
      INTEGER STATUS             ! Global status

*.
      IF ( STATUS .NE. SAI__OK ) RETURN
      IMG1_NCEL = ARRAY( N )
      END
* $Id$
