      SUBROUTINE IRA_SCSEP( IDA, SCS, EPOCH, STATUS )
*+
*  Name:
*     IRA_SCSEP

*  Purpose:
*     Get the sky coordinate system and epoch associated with an IRA
*     identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_SCSEP( IDA, SCS, EPOCH, STATUS )

*  Description:
*     The sky coordinate system and Julian epoch of observation
*     identified by IDA are returned.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     SCS = CHARACTER * ( * ) (Returned)
*        The sky coordinate system to which the projection specified by
*        IDA refers (see ID2 section "Sky Coordinates").
*     EPOCH = DOUBLE PRECISION (Returned)
*        The Julian epoch at which the observations were made.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common values.
*        ACM_EPOCH( IRA__MAX ) = DOUBLE PRECISION (Read)
*           The epoch of observations.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Read)
*           The sky coordinate system used by the projection.

*  Arguments Given:
      INTEGER IDA

*  Arguments Returned:
      CHARACTER SCS*(*)
      DOUBLE PRECISION EPOCH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Retrieve the name of the sky coordinate system and epoch from common.
      SCS = ACM_SCS( IDA )
      EPOCH = ACM_EPOCH( IDA )

*  If an error occurred, add a contextual message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_SCSEP_ERR1',
     :       'IRA_SCSEP: Unable to get sky coordinate system and epoch',
     :                 STATUS )
      END IF

      END
