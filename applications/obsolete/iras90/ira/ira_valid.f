      SUBROUTINE IRA_VALID( NVAL, FORWRD, SCS, IDA, IN1, IN2, OK,
     :                      STATUS )
*+
*  Name:
*     IRA_VALID

*  Purpose:
*     Check for valid coordinate data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_VALID( NVAL, FORWRD, SCS, IDA, IN1, IN2, OK, STATUS )

*  Description:
*     This routine can be used to check whether or not given input
*     coordinates would transform to valid output coordinates if
*     transformed using IRA_TRANS. This is faster than actually doing
*     the transformation using IRA_TRANS.
*
*  Arguments:
*     NVAL = INTEGER (Given)
*        The number of coordinate points to be transformed.
*     FORWRD = LOGICAL (Given)
*        If true then the forward mapping is used from image coordinate
*        to sky coordinate. Otherwise, the inverse mapping from sky
*        coordinate to image coordinates is used.
*     SCS = CHARACTER * ( * ) (Given)
*        The name of the sky coordinate system for input sky coordinate
*        positions. Any unambiguous abbreviation will do. This need not
*        be the same as the SCS identified by IDA. If FORWRD is true,
*        SCS is ignored, since if the given image coordinates transform
*        to a valid position in one sky coordinate system, then they
*        will transform to valid positions in all sky coordinate
*        systems (the reverse is not true since some projections do not
*        cover the entire sky).  See ID2 section "Sky Coordinates" for
*        more information on Sky Coordinate Systems. A blank value will
*        cause the system associated with IDA to be used.
*     IDA = INTEGER (Given)
*        The IRA identifier for the astrometry information.
*     IN1( NVAL ) = DOUBLE PRECISION (Given and Returned)
*        If FORWRD is true, then IN1 holds values of the first image
*        coordinate (X) on entry, and the corresponding U values on
*        exit. Otherwise IN1 holds values of the sky longitude on entry
*        and the corresponding local longitude on exit. See ID/2
*        appendix D for a description of the local sky coordinate
*        system, and the (U,V) coordinate system.
*     IN2( NVAL ) = DOUBLE PRECISION (Given and Returned)
*        If FORWRD is true, then IN2 holds values of the second image
*        coordinate (Y) on entry, and the corresponding V values on
*        exit. Otherwise IN2 holds values of the sky latitude on entry
*        and the corresponding local latitude on exit.
*     OK( NVAL ) = LOGICAL (Returned)
*        True if the corresponding input coordinates would transform to
*        valid output coordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-FEB-1992 (DSB):
*        Original version.
*     12-FEB-1993 (DSB):
*        Storage of locators in common removed.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common values.
*        ACM_EPOCH( IRA__MAX ) = DOUBLE PRECISION (Read)
*           Julian epoch of observation from the associated AS.
*        ACM_PROJN( IRA__MAX ) = CHARACTER (Read)
*           Full projection name from the associated AS.
*        ACM_PROJP( IRA__MAXP, IRA__MAX ) = DOUBLE PRECISION (Read)
*           Projection parameter values from the associated AS.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Read)
*           Full sky coordinate system (SCS) name from the associated
*           AS, with optional equinox specifier.

*  Arguments Given:
      INTEGER          NVAL
      LOGICAL          FORWRD
      CHARACTER        SCS*(*)
      INTEGER          IDA

*  Arguments Given and Returned:
      DOUBLE PRECISION IN1( NVAL )
      DOUBLE PRECISION IN2( NVAL )

*  Arguments Returned:
      LOGICAL OK( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   I                ! Loop count.
      INTEGER   NPREQ            ! No. of projection parameters required
      CHARACTER PROJ*(IRA__SZPRJ)! Full projection name.
      CHARACTER LSCS*(IRA__SZSCS)! Local copy of SCS.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )

*  If a blank SCS was given, use the value associated with IDA.
      IF( SCS .EQ. ' ' ) THEN
         LSCS = ACM_SCS( IDA )
      ELSE
         LSCS = SCS
      END IF

*  If the input values are sky coordinates, convert them to the
*  sky coordinate system used by the projection.
      IF( .NOT. FORWRD ) THEN
         CALL IRA_CONVT( NVAL, IN1, IN2, LSCS, ACM_SCS(IDA),
     :                   ACM_EPOCH(IDA), IN1, IN2, STATUS )
      END IF

*  Identify the projection and get the enumber of projection parameters.
      CALL IRA1_CHPRJ( ACM_PROJN( IDA ), PROJ, NPREQ, STATUS )

*  Call IRA1_IVALD to do the checking.
      CALL IRA1_IVALD( NVAL, FORWRD, PROJ, NPREQ, ACM_PROJP(1,IDA),
     :                 IN1, IN2, OK, STATUS )

*  If an error occurred, give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_VALID_ERR1',
     :                 'IRA_VALID: Unable to check coordinate validity',
     :                 STATUS )
      END IF

      END
