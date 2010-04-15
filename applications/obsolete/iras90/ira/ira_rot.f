      SUBROUTINE IRA_ROT( IDA, ROT, XC, YC, STATUS )
*+
*  Name:
*     IRA_ROT

*  Purpose:
*     Corrects astrometry information for rotation of image coordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_ROT( IDA, ROT, XC, YC, STATUS )

*  Description:
*     This routine modifies the astrometry information identified by
*     IDA to incorporate the effect of a rotation of the image
*     coordinates about an arbitrary centre. Such a transformation
*     preserves the intrinsic properties of the original projection.
*
*     Note, once this routine has been called, IRA_EXPRT or IRA_WRITE
*     should be called if required to save the modified astrometry
*     information in an NDF or HDS object.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information to be
*        modified.
*     ROT = DOUBLE PRECISION (Given)
*        The angle through which the image coordinate frame has been
*        rotated, in radians. Positive rotation is in the same sense as
*        rotation from north to east.
*     XC = DOUBLE PRECISION (Given)
*        The X image coordinate of the centre of rotation.
*     YC = DOUBLE PRECISION (Given)
*        The Y image coordinate of the centre of rotation.
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
      DOUBLE PRECISION ROT
      DOUBLE PRECISION XC
      DOUBLE PRECISION YC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BADARG*6         ! The bad argument name.
      DOUBLE PRECISION COSROT    ! COS of the rotation angle.
      DOUBLE PRECISION DX        ! X offset from centre of rotation to
                                 ! reference point.
      DOUBLE PRECISION DY        ! Y offset from centre of rotation to
                                 ! reference point.
      DOUBLE PRECISION SINROT    ! SIN of the rotation angle.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Check that non of the supplied arguments are bad.
      BADARG = ' '
      IF( XC .EQ. VAL__BADD ) BADARG = 'XC'
      IF( YC .EQ. VAL__BADD ) BADARG = 'YC'
      IF( ROT .EQ. VAL__BADD ) BADARG = 'ROT'

      IF( BADARG .NE. ' ' ) THEN
         STATUS = IRA__BADPA
         CALL MSG_SETC( 'A', BADARG )
         CALL ERR_REP( 'IRA_ROT_ERR1',
     :                 'IRA_ROT: BAD value supplied for argument ^A',
     :                 STATUS )
         GO TO 999
      END IF

*  Calculate things needed to fin the new image coordinates of the
*  reference point.
      DX = ACM_PROJP( 3, IDA ) -  XC
      DY = ACM_PROJP( 4, IDA ) -  YC
      COSROT = COS( ROT )
      SINROT = SIN( ROT )

*  Modify the projection parameters stored in common
      ACM_PROJP( 3, IDA ) = DX*COSROT + DY*SINROT
      ACM_PROJP( 4, IDA ) = -DX*SINROT + DY*COSROT
      ACM_PROJP( 7, IDA ) = ACM_PROJP( 7, IDA ) + ROT

*  If an error has occured, report a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_ROT_ERR2',
     : 'IRA_ROT: Unable to correct astrometry information for rotation',
     :                  STATUS )
      END IF

      END
