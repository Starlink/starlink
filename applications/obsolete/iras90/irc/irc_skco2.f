      SUBROUTINE IRC_SKCO2( IDC, NVAL, SAMPLE, DETIND, ZFP, YFP, DPW1,
     :                      DPW2, DPW3, RW4, RA, DEC, STATUS )
*+
*  Name:
*     IRC_SKCO2

*  Purpose:
*     Find sky coordinates of a single focal plane position, at a set
*     of different times.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_SKCO2( IDC, NVAL, SAMPLE, DETIND, ZFP, YFP, DPW1,
*                     DPW2, DPW3, RW4, RA, DEC, STATUS )

*  Description:
*     This routine returns the Right Ascension and Declination values
*     of the given focal plane position, at times specified by the
*     given sample numbers and detector indices. See ID1 Appendix E for
*     a description of the focal plane (Z,Y) coordinate system.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     NVAL = INTEGER (Given)
*        The number of times at which the given focal plane position is
*        to be converted.
*     SAMPLE( NVAL ) = REAL (Given)
*        The fractional sample numbers at which to do the conversion.
*        If any sample is equal to the Starlink "BAD" value (VAL__BADR)
*        then the corresponding elements of the RA and DEC arrays are
*        returned bad.
*     DETIND( NVAL ) = INTEGER (Given)
*        The detector indices to which the given sample numbers refer.
*        These must be within the bounds of the second dimension of the
*        NDF.
*     ZFP = REAL (Given)
*        The focal plane Z coordinate value, in radians. If this is
*        equal to the Starlink "BAD" data value (VAL__BADR), then both
*        output arrays (RA and DEC) are filled with the bad value
*        VAL__BADD.
*     YFP = REAL (Given)
*        The focal plane Y coordinate value, in radians. If this is
*        equal to the Starlink "BAD" data value (VAL__BADR), then both
*        output arrays (RA and DEC) are filled with the bad value
*        VAL__BADD.
*     DPW1( NVAL ) = DOUBLE PRECISION (Returned)
*        Double precision workspace.
*     DPW2( NVAL ) = DOUBLE PRECISION (Returned)
*        Double precision workspace.
*     DPW3( NVAL ) = DOUBLE PRECISION (Returned)
*        Double precision workspace.
*     RW4( NVAL ) = REAL (Returned)
*        Real workspace.
*     RA( NVAL ) = DOUBLE PRECISION (Returned)
*        The Right Ascension (B1950 FK4) of the focal plane position,
*        (in radians) at each boresight position.
*     DEC( NVAL ) = DOUBLE PRECISION (Returned)
*        The Declination (B1950 FK4) of the focal plane position,
*        (in radians) at each boresight position.
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
      INCLUDE 'PRM_PAR'          ! Starlink BAD values.
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
      INTEGER NVAL
      REAL    SAMPLE( NVAL )
      INTEGER DETIND( NVAL )
      REAL    ZFP
      REAL    YFP

*  Arguments Returned:
      DOUBLE PRECISION DPW1( NVAL )
      DOUBLE PRECISION DPW2( NVAL )
      DOUBLE PRECISION DPW3( NVAL )
      REAL             RW4( NVAL )
      DOUBLE PRECISION RA( NVAL )
      DOUBLE PRECISION DEC( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A1        ! Sky longitude after moving out along
                                 ! the Y axis.
      DOUBLE PRECISION B1        ! Sky latitude after moving out along
                                 ! the Y axis.
      DOUBLE PRECISION ENDANG    ! Position angle of the trajectory at
                                 ! the end of a shift.
      INTEGER          I         ! Loop count.
      CHARACTER SCS*(IRA__SZSCS)! Sky Coordinate System used by
                                 ! IRC1_BPOSI.
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
         CALL ERR_REP( 'IRC_SKCO2_ERR1',
     :                 'IRC_SKCO2: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that each detector index is within the bounds of the NDF second
*  dimension.
      DO I = 1, NVAL
         IF( DETIND(I) .LT. CCM_DLOW( IDC ) .OR.
     :       DETIND(I) .GT. CCM_DHIGH( IDC ) ) THEN
            STATUS = IRC__BADDI
            CALL MSG_SETI( 'D', DETIND(I) )
            CALL MSG_SETI( 'DL', CCM_DLOW( IDC ) )
            CALL MSG_SETI( 'DH', CCM_DHIGH( IDC ) )
            CALL ERR_REP( 'IRC_SKCO2_ERR2',
     :        'IRC_SKCO2: Detector index ^D is out of bounds [^DL,^DH]',
     :                  STATUS )
            GO TO 999
         END IF
      END DO

*  Check that the input focal plane position is OK.
      IF( ZFP .EQ. VAL__BADR .OR. YFP .EQ. VAL__BADR ) THEN
         DO I = 1, NVAL
            RA(I) = VAL__BADD
            DEC(I) = VAL__BADD
         END DO
         GO TO 999
      END IF

*  Get the boresight positions scan angle at the requested sample. DPW1
*  holds the sky longitude, DPW2 holds the sky latitude,
*  DPW3 holds the scan angle, RW4 holds the scan speed.
      CALL IRC1_BPOSI( IDC, NVAL, SAMPLE, DETIND, SCS, DPW1, DPW2, DPW3,
     :                 RW4, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each boresight position.
      DO I = 1, NVAL

*  If the corresponding sample number is bad, set the output arrays bad.
         IF( SAMPLE( I ) .EQ. VAL__BADR ) THEN
            RA( I ) = VAL__BADD
            DEC( I ) = VAL__BADD

*  Otherwise, shift away from the boresight along the Y axis by the
*  distance specified by the focal plane Y coordinate value.
         ELSE
            CALL IRA_SHIFT( DPW1(I), DPW2(I), DPW3(I), DBLE( YFP ),
     :                      A1, B1, ENDANG, STATUS )

*  Now shift along the Z axis by the distance specified by the focal
*  plane Z coordinate value.
            CALL IRA_SHIFT( A1, B1, ENDANG - IRC__PIBY2, DBLE( ZFP ),
     :                      RA(I), DEC(I), ENDANG, STATUS )

         END IF

*  Do the next boresight position.
      END DO

*  Convert the sky positions to equatorial (B1950) coordinates.
      CALL IRA_CONVT( NVAL, RA, DEC, SCS, 'EQUATORIAL(B1950)',
     :                IRA__IRJEP, RA, DEC, STATUS )

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_SKCO2_ERR3',
     :     'IRC_SKCO2: Unable to convert from focal plane to sky '//
     :     'coordinates ', STATUS )
      END IF

      END
