      SUBROUTINE IRC_SKCO1( IDC, SAMPLE, DETIND, NVAL, ZFP, YFP, RA,
     :                      DEC, STATUS )
*+
*  Name:
*     IRC_SKCO1

*  Purpose:
*     Find sky coordinates of a set of focal plane positions at
*     a time specified by a given sample.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_SKCO1( IDC, SAMPLE, DETIND, NVAL, ZFP, YFP, RA, DEC,
*                     STATUS )

*  Description:
*     This routine returns the Right Ascension and Declination
*     of each point in a list of focal plane positions, at the
*     moment specified by the given sample number and detector index.
*     See ID1 Appendix E for a description of the focal plane (Z,Y)
*     coordinate system.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     SAMPLE = REAL (Given)
*        The fractional sample number at which to do the conversion.
*        If this has the Starlink "BAD" value (VAL__BADR) then all
*        elements of the RA and DEC arrays are returned bad.
*     DETIND = INTEGER (Given)
*        The detector index to which the given sample number refers.
*        This must be within the bounds of the second dimension of the
*        NDF.
*     NVAL = INTEGER (Given)
*        The number of focal plane positions to be converted.
*     ZFP( NVAL ) = REAL (Given)
*        The focal plane Z coordinate values, in radians. If any value
*        is equal to the Starlink "BAD" value (VAL__BADR) then the
*        corresponding value in both the RA and DEC arrays are set to
*        the bad value VAL_BADD.
*     YFP( NVAL ) = REAL (Given)
*        The focal plane Y coordinate values, in radians. If any value
*        is equal to the Starlink "BAD" value (VAL__BADR) then the
*        corresponding value in both the RA and DEC arrays are set to
*        the bad value VAL__BADD.
*     RA( NVAL ) = DOUBLE PRECISION (Returned)
*        The Right Ascension (B1950 FK4) of each focal plane positions,
*        in radians.
*     DEC( NVAL ) = DOUBLE PRECISION (Returned)
*        The Declination (B1950 FK4) of each focal plane positions,
*        in radians.
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
      INCLUDE 'PRM_PAR'          ! Starlink bad values.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error values.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest detector index in the DATA array.

*  Arguments Given:
      INTEGER IDC
      REAL    SAMPLE
      INTEGER DETIND
      INTEGER NVAL
      REAL    ZFP( NVAL )
      REAL    YFP( NVAL )

*  Arguments Returned:
      DOUBLE PRECISION RA( NVAL )
      DOUBLE PRECISION DEC( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION BA        ! Sky longitude value at boresight.
      DOUBLE PRECISION BA1       ! Sky longitude value after moving out
                                 ! along the Y axis.
      DOUBLE PRECISION BB        ! Sky latitude value at boresight.
      DOUBLE PRECISION BB1       ! Sky latitude value after moving out
                                 ! along the Y axis.
      DOUBLE PRECISION ENDANG    ! Position angle of the trajectory at
                                 ! the end of a shift.
      INTEGER          I         ! Loop count.
      CHARACTER SCS*(IRA__SZSCS)! Sky Coordinate System used by IRC1_BPOSI
      REAL             SPEED     ! Scan speed.
      DOUBLE PRECISION YPA       ! Position angle (from north through
                                 ! east) of the +ve Y axis.
      DOUBLE PRECISION ZPA       ! Position angle (from north through
                                 ! east) of the +ve Z axis.
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
         CALL ERR_REP( 'IRC_SKCO1_ERR1',
     :                 'IRC_SKCO1: Invalid IRC identifier supplied',
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
         CALL ERR_REP( 'IRC_SKCO1_ERR2',
     :        'IRC_SKCO1: Sample number ^S is out of bounds [^SL,^SH]',
     :                  STATUS )
         GO TO 999
      END IF

*  If the given sample number is bad, set the entire RA and DEC arrays
*  bad, and return.
      IF( SAMPLE .EQ. VAL__BADR ) THEN

         DO I = 1, NVAL
            RA( I ) = VAL__BADD
            DEC( I ) = VAL__BADD
         END DO

         GO TO 999

      END IF

*  Get the boresight position and scan angle at the requested sample.
      CALL IRC1_BPOSI( IDC, 1, SAMPLE, DETIND, SCS, BA, BB, YPA,
     :                 SPEED, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each focal plane position. If either focal plane
*  coordinate is bad, both RA and DEC are set bad.
      DO I = 1, NVAL

         IF( YFP(I) .NE. VAL__BADR .AND. ZFP(I) .NE. VAL__BADR ) THEN

*  Shift away from the boresight along the Y axis by the distance
*  specified by the focal plane Y coordinate value.
            CALL IRA_SHIFT( BA, BB, YPA, DBLE( YFP(I) ), BA1, BB1,
     :                      ENDANG, STATUS )

*  Find the position angle of the Z axis at the new position.
            ZPA = ENDANG - IRA__PIBY2

*  Now shift along the Z axis by the distance specified by the focal
*  plane Z coordinate value.
            CALL IRA_SHIFT( BA1, BB1, ZPA, DBLE( ZFP(I) ), RA(I),
     :                      DEC(I), ENDANG, STATUS )

         ELSE
            RA(I) = VAL__BADD
            DEC(I) = VAL__BADD

         END IF

*  Do the next focal plane position.
      END DO

*  Convert the sky positions to equatorial (B1950) coordinates.
      CALL IRA_CONVT( NVAL, RA, DEC, SCS, 'EQUATORIAL(B1950)',
     :                IRA__IRJEP, RA, DEC, STATUS )

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_SKCO1_ERR3',
     :        'IRC_SKCO1: Unable to convert from focal plane to sky '//
     :        'coordinates.', STATUS )
      END IF

      END
