      SUBROUTINE IRA1_ASRES( LOC, STATUS )
*+
*  Name:
*     IRA1_ASRES

*  Purpose:
*     Reset an astrometry structure to the UNDEFINED state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_ASRES( LOC, STATUS )

*  Description:
*     The STATE component of the astrometry structure (AS) is given the
*     value "UNDEFINED". This will cause other IRA routines to ignore
*     any values stored for the other AS components.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the astrometry structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-1990 (DSB):
*        Original version.
*     24-APR-1991 (DSB):
*        Name changed from IRA_$ASRES TO IRA1_ASRES
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the STATE component of the supplied AS, and set its value to
*  UNDEFINED.
      CALL CMP_PUT0C( LOC, 'STATE', 'UNDEFINED', STATUS )

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA1_ASRES_ERR1',
     : 'IRA1_ASRES: Unable to reset an astrometry structure to the '//
     : 'UNDEFINED state', STATUS )
      END IF

      END
