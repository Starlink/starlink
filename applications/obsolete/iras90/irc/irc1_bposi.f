      SUBROUTINE IRC1_BPOSI( IDC, NVAL, SAMPLE, DETIND, SCS, A, B,
     :                       ANGLE, SPEED, STATUS )
*+
*  Name:
*     IRC1_BPOSI

*  Purpose:
*     Returns boresight positions at a set of detector samples, with
*     no argument verification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_BPOSI( IDC, NVAL, SAMPLE, DETIND, SCS, A, B, ANGLE,
*                      SPEED, STATUS )

*  Description:
*     This routine provides the functionality for routine IRC_BPOS.
*     No checks are made on the validity of the input arguments. The
*     boresight positions and scan angles are returned in a Sky
*     Coordinate System which may vary with CRDD-type. The SCS actually
*     used is returned in argument SCS.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     NVAL = INTEGER (Given)
*        The number of samples in the input and output lists.
*     SAMPLE( NVAL ) = REAL (Given)
*        A list of fractional sample numbers. If any sample number is
*        BAD, then the corresponding elements in the output arrays are
*        set BAD.
*     DETIND( NVAL ) = INTEGER (Given)
*        A list of detector indices.
*     SCS = CHARACTER * ( * ) (Returned)
*        The Sky Coordinate System in which the boresight positions and
*        angles are returned. See the IRA_ documentation (ID/1) for more
*        information about Sky Coordinate Systems. The variable supplied
*        for argument SCS should have a declared length equal to the
*        symbolic constant IRA__SZSCS.
*     A( NVAL ) = DOUBLE PRECISION (Returned)
*        An array holding the sky longitude value of the
*        boresight at the moment each sample specified in the input
*        lists was taken (radians). The values are in the Sky Coordinate
*        System returned in argument SCS.
*     B( NVAL ) = DOUBLE PRECISION (Returned)
*        An array holding the sky latitude value of the
*        boresight at the moment each sample specified in the input
*        lists was taken (radians). The values are in the Sky Coordinate
*        System returned in argument SCS.
*     ANGLE( NVAL ) = DOUBLE PRECISION (Returned)
*        The scan angle at the boresight. This is measured from north
*        (in the Sky Coordinate System specified by SCS) to the focal
*        plane Y axis.  The angle is in radians and is measured positive
*        in the same sense as rotation from north to east.
*     SPEED( NVAL ) = REAL (Returned)
*        The scan speed in radians per second. Positive values imply
*        that sources move in the positive focal plane Y direction (i.e
*        in the "with-survey" direction). Negative values imply that
*        sources move in the negative Y direction (i.e. "anti-survey").
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*                (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
*     {original_version_entry}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_TYPE( IRC__MAX ) = CHARACTER (Write)
*           HDS type of the DETAILS component.

*  Arguments Given:
      INTEGER IDC
      INTEGER NVAL
      REAL    SAMPLE( NVAL )
      INTEGER DETIND( NVAL )

*  Arguments Returned:
      CHARACTER SCS*(*)
      DOUBLE PRECISION A( NVAL )
      DOUBLE PRECISION B( NVAL )
      DOUBLE PRECISION ANGLE( NVAL )
      REAL    SPEED( NVAL )

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call an appropriate routine for each CRDD type.
      IF( CCM_TYPE( IDC ) .EQ. 'SURVEY_BSIGHT' ) THEN
         CALL IRC1_BPSSB( IDC, NVAL, SAMPLE, DETIND, SCS, A, B,
     :                    ANGLE, SPEED, STATUS )

*  If the CRDD type is unrecognised, give an error report.
      ELSE
         STATUS = IRC__BADTY
         CALL MSG_SETC( 'T', CCM_TYPE( IDC ) )
         CALL ERR_REP( 'IRC1_BPOSI_ERR1',
     :                 'IRC1_BPOSI does not yet support ^T data',
     :                 STATUS )
      END IF


      END
