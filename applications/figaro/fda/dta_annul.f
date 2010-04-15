      SUBROUTINE DTA_ANNUL( LOC, STATUS )
*+
*  Name:
*     DTA_ANNUL

*  Purpose:
*     Anull an HDS locator.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA_ANNUL( LOC, DTA_STATUS )

*  Description:
*     This routine annuls an HDS locator obtained by DTA_LOC or any HDS
*     routine. It simply makes a call to DAT_ANNUL, but the status
*     argument here is a non-inherited DTA status rather than an
*     inherited Starlink status.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        The HDS locator.
*     DTA_STATUS = INTEGER (Returned)
*        The DTA status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     05 Mar 1996 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Call the work routine.
      CALL DAT_ANNUL( LOC, STATUS )

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
