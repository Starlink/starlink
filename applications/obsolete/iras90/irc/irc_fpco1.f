      SUBROUTINE IRC_FPCO1( IDC, SAMPLE, DETIND, NVAL, RA, DEC, ZFP,
     :                      YFP, STATUS )
*+
*  Name:
*     IRC_FPCO1

*  Purpose:
*     Find focal plane coordinates of many sky positions at a time
*     determined by a given sample.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_FPCO1( IDC, SAMPLE, DETIND, NVAL, RA, DEC, ZFP, YFP,
*                     STATUS )

*  Description:
*     This routine returns the focal plane Z and Y coordinate
*     values (see ID1 Appendix E) of a list of sky positions, at the
*     moment the specified sample was taken.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     SAMPLE = REAL (Given)
*        The fractional sample number. If this has the Starlink "BAD"
*        value, then all returned focal plane coordinates are set bad.
*     DETIND = INTEGER (Given)
*        The detector index to which the sample number refers.
*     NVAL = INTEGER (Given)
*        The number of sky positions to be converted.
*     RA( NVAL ) = DOUBLE PRECISION (Given)
*        The Right Ascension values (B1950 FK4) of the sky positions,
*        in radians. If a Starlink "BAD" value (VAL__BADD) is found,
*        the corresponding element of array FPCO is set to the bad
*        value.
*     DEC( NVAL ) = DOUBLE PRECISION (Given)
*        The Declination values (B1950 FK4) of the sky positions, in
*        radians. If a Starlink "BAD" value (VAL__BADD) is found, the
*        corresponding element of array FPCO is set to the bad value.
*     ZFP( NVAL ) = REAL (Returned)
*        The focal plane Z coordinate values, in radians.
*     YFP( NVAL ) = REAL (Returned)
*        The focal plane Y coordinate values, in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-FEB-1991 (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error values.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_SLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest sample number in the DATA array.
*        CCM_SHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest sample number in the DATA array.
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest detector index in the DATA array.

*  Arguments Given:
      INTEGER IDC
      REAL    SAMPLE
      INTEGER DETIND
      INTEGER NVAL
      DOUBLE PRECISION RA( NVAL )
      DOUBLE PRECISION DEC( NVAL )

*  Arguments Returned:
      REAL    ZFP( NVAL )
      REAL    YFP( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION BA        ! Sky longitude at the boresight.
      DOUBLE PRECISION BANGLE    ! Position angle of the +ve Y axis
      DOUBLE PRECISION BB        ! Sky latitude at the boresight.
      DOUBLE PRECISION BDEC      ! Boresight DEC.
      DOUBLE PRECISION BRA       ! Boresight RA.
      DOUBLE PRECISION DYFP      ! DOUBLE PRECISION version of YFP
      DOUBLE PRECISION DZFP      ! DOUBLE PRECISION version of ZFP
      DOUBLE PRECISION GDEC      ! DEC of given point.
      DOUBLE PRECISION GRA       ! RA of given point.
      INTEGER          I         ! Loop count.
      CHARACTER SCS*(IRA__SZSCS)! Sky Coordinate System used by
                                 ! IRC_BPOSI.
      REAL             SPEED     ! Scan speed.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied IRC identifier is valid. If not, report an error
*  and quit.
      IF( IDC .LE. 0 .OR. IDC .GT. IRC__MAX ) THEN
         STATUS = IRC__INVID

      ELSE IF( .NOT. CCM_VALID( IDC ) ) THEN
         STATUS = IRC__INVID

      END IF

      IF( STATUS .EQ. IRC__INVID ) THEN
         CALL ERR_REP( 'IRC_FPCO1_ERR1',
     :                 'IRC_FPCO1: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that the detector index is within the bounds of the NDF second
*  dimension.
      IF( DETIND .LT. CCM_DLOW( IDC ) .OR.
     :    DETIND .GT. CCM_DHIGH( IDC ) ) THEN
         STATUS = IRC__BADDI
         CALL MSG_SETI( 'D', DETIND )
         CALL MSG_SETI( 'DL', CCM_DLOW( IDC ) )
         CALL MSG_SETI( 'DH', CCM_DHIGH( IDC ) )
         CALL ERR_REP( 'IRC_FPCO1_ERR2',
     :        'IRC_FPCO1: Detector index ^D is out of bounds [^DL,^DH]',
     :                  STATUS )
         GO TO 999
      END IF

*  If the sample number is bad, set all output focal plane coordinates
*  bad, and return.
      IF( SAMPLE .EQ. VAL__BADR ) THEN

         DO I = 1, NVAL
            ZFP( I ) = VAL__BADR
            YFP( I ) = VAL__BADR
         END DO

         GO TO 999

      END IF

*  Get the boresight position and scan angle at the requested sample.
      CALL IRC1_BPOSI( IDC, 1, SAMPLE, DETIND, SCS, BA, BB, BANGLE,
     :                 SPEED, STATUS )

*  Convert the boresight position and scan angle to equatorial (B1950)
*  coordinates.
      CALL IRA_PACON( 1, BA, BB, BANGLE, SCS, 'EQUATORIAL(B1950)',
     :                IRA__IRJEP, BRA, BDEC, BANGLE, STATUS)

      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each good input sky position.
      DO I = 1, NVAL
         GRA = RA(I)
         GDEC = DEC(I)
         IF( GRA .NE. VAL__BADD .AND. GDEC .NE. VAL__BADD ) THEN

*  Resolve the given sky position into components parallel and
*  perpendicular to the focal plane Y axis.
            CALL IRA_DIST2( BRA, BDEC, BANGLE, GRA, GDEC,
     :                      DYFP, DZFP, STATUS )

*  The perpendicular component returned by IRA_DIST2 is in the opposite
*  sense to that of the focal plane. Invert it to get the focal plane Z
*  value.
            ZFP( I ) = -REAL( DZFP )
            YFP( I ) = REAL( DYFP )

*  If the input skyt position was bad, set the output values bad.
         ELSE
            ZFP( I ) = VAL__BADR
            YFP( I ) = VAL__BADR

         END IF

      END DO

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_FPCO1_ERR3',
     :      'IRC_FPCO1: Unable to convert from sky to focal plane '//
     :      'coordinates ', STATUS )
      END IF

      END
