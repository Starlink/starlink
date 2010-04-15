      SUBROUTINE IRA_MOVE( IDA, SHIFTX, SHIFTY, STATUS )
*+
*  Name:
*     IRA_MOVE

*  Purpose:
*     Corrects astrometry information for shift of image coordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_MOVE( IDA, SHIFTX, SHIFTY, STATUS )

*  Description:
*     This routine modifies the astrometry information identified by
*     IDA to incorporate the effect of a shift of the origin of image
*     coordinates. Such a transformation preserves the intrinsic
*     properties of the original projection.
*
*     Note, once this routine has been called, IRA_EXPRT or IRA_WRITE
*     should be called if required to save the modified astrometry
*     information in an NDF or HDS object.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information to be
*        modified.
*     SHIFTX = DOUBLE PRECISION (Given)
*        The shift in X of the origin of the image coordinates, in
*        pixels.
*     SHIFTY = DOUBLE PRECISION (Given)
*        The shift in Y of the origin of the image coordinates, in
*        pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-OCT-1992 (DSB):
*        Original version.
*     12-FEB-1993 (DSB):
*        Modified information no longer saved in an NDF or HDS object.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA error constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_PROJP( IRA__MAXP, IRA__MAX ) = DOUBLE PRECISION (Read and Write)
*           Projection parameters.

*  Arguments Given:
      INTEGER IDA
      DOUBLE PRECISION SHIFTX
      DOUBLE PRECISION SHIFTY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BADARG*6         ! The bad argument name.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Check that non of the supplied arguments are bad.
      BADARG = ' '
      IF( SHIFTX .EQ. VAL__BADD ) BADARG = 'SHIFTX'
      IF( SHIFTY .EQ. VAL__BADD ) BADARG = 'SHIFTY'

      IF( BADARG .NE. ' ' ) THEN
         STATUS = IRA__BADPA
         CALL MSG_SETC( 'A', BADARG )
         CALL ERR_REP( 'IRA_MOVE_ERR1',
     :                 'IRA_MOVE: BAD value supplied for argument ^A',
     :                 STATUS )
         GO TO 999
      END IF

*  Modify the projection parameters stored in common
      ACM_PROJP( 3, IDA ) = ACM_PROJP( 3, IDA ) + SHIFTX
      ACM_PROJP( 4, IDA ) = ACM_PROJP( 4, IDA ) + SHIFTY

*  If an error has occured, report a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_MOVE_ERR2',
     :            'IRA_MOVE: Unable to modify astrometry information',
     :                  STATUS )
      END IF

      END
