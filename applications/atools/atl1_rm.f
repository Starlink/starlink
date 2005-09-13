      SUBROUTINE ATL1_RM( FILE, STATUS )
*+
*  Name:
*     ATL1_RM

*  Purpose:
*     Remove a file.

*  Description:
*     This subroutine calls the "PSX_REMOVE" RTL function to remove a
*     specified file. No error occurs if the file cannot be removed for
*     any reason.

*  Parameters:
*     FILE = CHARACTER * ( * ) (Given)
*        The path to the file.
*     STATUS = INTEGER (Given and Returned)
*        The inherited global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version.
*     13-SEP-2005 (TIMJ):
*        Rewritten in Fortran to call PSX_REMOVE
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER *(*) FILE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PSXSTAT

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Local status
      PSXSTAT = SAI__OK

*     New error context
      CALL ERR_MARK

*     Remove the file
      CALL PSX_REMOVE( FILE, PSXSTAT )

*     Clear bad status
      IF (PSXSTAT .NE. SAI__OK) CALL ERR_ANNUL( STATUS )

*     Reset error context
      CALL ERR_RLSE

      END
