      SUBROUTINE IRM_DELOB( LOC, STATUS )
*+
*  Name:
*     IRM_DELOB

*  Purpose:
*     Delete an HDS object given its locator.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_DELOB( LOC, STATUS )

*  Description:
*     The parent object is found, together with the name of the object
*     to be deleted. The locator to the object is anulled and the object
*     deleted, together with all lower level components. If the object
*     to be deleted does not have a parent object, then an error occurs.

*  Arguments:
*     LOC = CHARACTER (Given)
*        An HDS locator to the object to be deleted, NOT the enclosing 
*        object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1990 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants

*  Arguments Given:
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME*(DAT__SZNAM) ! Name of object to be deleted.
      CHARACTER PARLOC*(DAT__SZLOC)! Locator to parent object.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a locator to the parent object.
      CALL DAT_PAREN( LOC, PARLOC, STATUS )

*  Find the name of the object to be deleted.
      CALL DAT_NAME( LOC, NAME, STATUS )

*  Annull the locator.
      CALL DAT_ANNUL( LOC, STATUS )

*  Erase the object and all lower level components.
      CALL DAT_ERASE( PARLOC, NAME, STATUS )

      END
* $Id$
