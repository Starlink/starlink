      SUBROUTINE IMG1_CFREE( POINT, STATUS )
*+
*  Name:
*     IMG1_CFREE

*  Purpose:
*     Frees memory allocated by IMG1_CALLO (UNIX version).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_CFREE( POINT, STATUS )

*  Description:
*     This routine frees memory allocated by the routine IMG1_MALLO.
*     It attempts to free memory even if status is set on entry.

*  Arguments:
*     POINT = INTEGER (Given)
*       Pointer to the memory which is to be deallocated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - UNIX specific. Does very little.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
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
      INCLUDE 'IMG_CONST'        ! IMG internal constants

*  Arguments Given:
      INTEGER POINT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Start a begin-end error block.
      CALL ERR_BEGIN( STATUS )

*  Attempt to free the memory.
      CALL PSX_FREE( POINT, STATUS )
      POINT = IMG__NOPTR

*  End of begin-end error block.
      CALL ERR_END( STATUS )

      END
* $Id$
