      SUBROUTINE IRA_PIXSZ( IDA, PIXSIZ, STATUS )
*+
*  Name:
*     IRA_PIXSZ

*  Purpose:
*     Get nominal pixel size.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_PIXSZ( IDA, PIXSIZ, STATUS )

*  Description:
*     The nominal pixel dimensions stored in the astrometry information
*     identified by IDA are returned.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     PIXSIZ( 2 ) = DOUBLE PRECISION (Returned)
*        The nominal pixel dimensions, in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-1992 (DSB):
*        Original version.
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
*        ACM_PROJP( IRA__MAXP, IRA__MAX ) = DOUBLE PRECISION (Read)
*           Projection parameters.

*  Arguments Given:
      INTEGER IDA

*  Arguments Returned:
      DOUBLE PRECISION PIXSIZ( 2 )

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Retrieve the nominal pixel dimensions.
      PIXSIZ( 1 ) = ACM_PROJP( 5, IDA )
      PIXSIZ( 2 ) = ACM_PROJP( 6, IDA )

*  If an error occurred, add a contextual message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_PIXSZ_ERR1',
     :              'IRA_PIXSZ: Unable to get nominal pixel dimensions',
     :                 STATUS )
      END IF

      END
