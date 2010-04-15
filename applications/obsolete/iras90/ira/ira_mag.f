      SUBROUTINE IRA_MAG( IDA, MAGX, MAGY, XC, YC, STATUS )
*+
*  Name:
*     IRA_MAG

*  Purpose:
*     Corrects astrometry information for magnification of image
*     coordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_MAG( IDA, MAGX, MAGY, XC, YC, STATUS )

*  Description:
*     This routine modifies the astrometry information identified by
*     IDA to incorporate the effect of magnification of the image
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
*     MAGX = DOUBLE PRECISION (Given)
*        The factor by which the pixel size in the X direction has been
*        decreased (i.e. a value of 2.0 results is given if the pixel
*        size has been halved).
*     MAGY = DOUBLE PRECISION (Given)
*        The factor by which the pixel size in the Y direction has been
*        decreased.
*     XC = DOUBLE PRECISION (Given)
*        The X image coordinate of the centre of magnification.
*     YC = DOUBLE PRECISION (Given)
*        The Y image coordinate of the centre of magnification.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-SEP-1992 (DSB):
*        Original version.
*     12-FEB-1993 (DSB):
*        Modified information no longer saved in an NDF or HDS object.
*     {enter_further_changes_here}

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
      DOUBLE PRECISION MAGX
      DOUBLE PRECISION MAGY
      DOUBLE PRECISION XC
      DOUBLE PRECISION YC

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

*  Check that non of the other arguments are bad.
      BADARG = ' '
      IF( MAGX .EQ. VAL__BADD ) BADARG = 'MAGX'
      IF( MAGY .EQ. VAL__BADD ) BADARG = 'MAGY'

      IF( BADARG .NE. ' ' ) THEN
         STATUS = IRA__BADPA
         CALL MSG_SETC( 'A', BADARG )
         CALL ERR_REP( 'IRA_MAG_ERR1',
     :                 'IRA_MAG: BAD value supplied for argument ^A',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that the magnification factors are not zero.
      BADARG = ' '
      IF( MAGX. EQ. 0.0 ) BADARG = 'X'
      IF( MAGY. EQ. 0.0 ) BADARG = 'Y'

      IF( BADARG .NE. ' ' ) THEN
         STATUS = IRA__BADPA
         CALL MSG_SETC( 'A', BADARG )
         CALL ERR_REP( 'IRA_MAG_ERR2',
     :                 'IRA_MAG: ^A magnification factor is zero.',
     :                 STATUS )
         GO TO 999
      END IF

*  Modify the projection parameters stored in common
      ACM_PROJP( 3, IDA ) = ( ACM_PROJP( 3, IDA ) - XC )*MAGX + XC
      ACM_PROJP( 4, IDA ) = ( ACM_PROJP( 4, IDA ) - YC )*MAGY + YC
      ACM_PROJP( 5, IDA ) = ACM_PROJP( 5, IDA )/MAGX
      ACM_PROJP( 6, IDA ) = ACM_PROJP( 6, IDA )/MAGY

*  If an error has occured, report a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_MAG_ERR3',
     :        'IRA_MAG: Unable to correct astrometry information for '//
     :        'magnification', STATUS )
      END IF

      END
