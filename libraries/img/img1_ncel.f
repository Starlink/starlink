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
