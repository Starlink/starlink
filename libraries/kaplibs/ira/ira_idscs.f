      SUBROUTINE IRA_IDSCS( IDA, SCS, STATUS )
*+
*  Name:
*     IRA_IDSCS

*  Purpose:
*     Get the Sky Coordinate System associated with an IRA identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_IDSCS( IDA, SCS, STATUS )

*  Description:
*     This routine returns the Sky Coordinate System associated with an 
*     IRA identifier.

*  Arguments:
*     IDA = INTEGER (Given)
*        The IRA identifier for the astrometry information.
*     SCS = CHARACTER * ( * ) (Returned)
*        On exit, contains the full version of the sky coordinate system
*        associated with the IRA identifier. The supplied variable should 
*        have a declared length given by symbolic constant IRA__SZSCS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-OCT-2004 (DSB):
*        Original version. 
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common values.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Read)
*           Full sky coordinate system (SCS) name from the associated
*           AS, with optional equinox specifier.

*  Arguments Given:
      INTEGER          IDA

*  Arguments Returned:
      CHARACTER SCS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Store the required SCS string.
         SCS = ACM_SCS( IDA ) 

*  If an error occurred, give a context message.
      ELSE
         CALL ERR_REP( 'IRA_IDSCS_ERR1', 'IRA_IDSCS: Unable to get '//
     :                 'the SCS for an IRA identifier', STATUS )
      END IF

      END
