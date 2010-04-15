      SUBROUTINE DTA1_THERE( PATH, EXIST, STATUS )
*+
*  Name:
*     DTA1_THERE

*  Purpose:
*     Check whether the specified structure exists.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA1_THERE( PATH, EXIST, STATUS )

*  Description:
*     This routine checks whether the structure identified by the given
*     DTA structure name exists.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The DTA structure name.
*     EXIST = LOGICAL (Returned)
*        Whether the structure exists.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     29 Feb 1996 (hme):
*        Original version.
*     05 Mar 1996 (hme):
*        Allow for temporary HDS structure during creation by structure
*        definition.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Arguments Given:
      CHARACTER * ( * ) PATH

*  Arguments Returned:
      LOGICAL EXIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  We want to use DTA1_LOC and have to translate its status value into
*  the returned argument. The simplest approach is to locate the object
*  in question, and if that fails, to assume that it is not there.
      CALL DTA1_LOC( PATH, LOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         EXIST = .FALSE.
      ELSE
         CALL DAT_ANNUL( LOC, STATUS )
         EXIST = .TRUE.
      END IF

*  Return.
      END
