      SUBROUTINE IRC_FPCO2( IDC, NVAL, SAMPLE, DETIND, RA, DEC, DPW1,
     :                      DPW2, DPW3, RW4, ZFP, YFP, STATUS )
*+
*  Name:
*     IRC_FPCO2

*  Purpose:
*     Find focal plane coordinates of a single sky position at a set of
*     different times.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_FPCO2( IDC, NVAL, SAMPLE, DETIND, RA, DEC, DPW1,
*                     DPW2, DPW3, RW4, ZFP, YFP, STATUS )

*  Description:
*     This routine returns the focal plane Z and Y coordinate
*     values of a given sky position, at a set of different times,
*     specified by a set of sample numbers and detector indices.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     NVAL = INTEGER (Given)
*        The number of samples at which to convert the given sky
*        position.
*     SAMPLE( NVAL ) = REAL (Given)
*        The fractional sample numbers at which to do the conversion.
*        If any sample number has the Starlink "BAD" value (VAL__BADR),
*        then the corresponding element of the array FPCO is returned
*        bad.
*     DETIND( NVAL ) = INTEGER (Given)
*        The detector indices to which the sample numbers refer. These
*        must all be within the bounds of the second dimension of the
*        NDF.
*     RA = DOUBLE PRECISION (Given)
*        The Right Ascension (B1950 FK4) of the sky position, in
*        radians. If this is equal to the Starlink "BAD" value
*        (VAL__BADD), then the FPCO array is returned full of bad
*        values.
*     DEC = DOUBLE PRECISION (Given)
*        The Declination (B1950 FK4) of the sky position. If this is
*        equal to the Starlink "BAD" value (VAL__BADD), then the FPCO
*        array is returned full of bad values.
*     DPW1( NVAL ) = DOUBLE PRECISION (Given and Returned)
*        Double precision workspace.
*     DPW2( NVAL ) = DOUBLE PRECISION (Given and Returned)
*        Double precision workspace.
*     DPW3( NVAL ) = DOUBLE PRECISION (Given and Returned)
*        Double precision workspace.
*     RW4( NVAL ) = REAL (Given and Returned)
*        Real workspace.
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
      DOUBLE PRECISION RA
      DOUBLE PRECISION DEC

*  Arguments Returned:
      DOUBLE PRECISION DPW1( NVAL )
      DOUBLE PRECISION DPW2( NVAL )
      DOUBLE PRECISION DPW3( NVAL )
      REAL    RW4( NVAL )
      REAL    ZFP( NVAL )
      REAL    YFP( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION  A1       ! sky longitude value of given sky
                                 ! position.
      DOUBLE PRECISION  B1       ! sky latitude value of given sky
                                 ! position.
      DOUBLE PRECISION  DYFP     ! DOUBLE PRECISION version of YFP.
      DOUBLE PRECISION  DZFP     ! DOUBLE PRECISION version of ZFP.
      INTEGER           I        ! Loop count.
      CHARACTER SCS*(IRA__SZSCS)! Sky Coordinate System used by IRC1_BPOSI
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
         CALL ERR_REP( 'IRC_FPCO2_ERR1',
     :                 'IRC_FPCO2: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that the detector indices are within the bounds of the NDF
*  second dimension.
      DO I = 1, NVAL
         IF( DETIND( I ) .LT. CCM_DLOW( IDC ) .OR.
     :       DETIND( I ) .GT. CCM_DHIGH( IDC ) ) THEN
            STATUS = IRC__BADDI
            CALL MSG_SETR( 'D', DETIND(I) )
            CALL MSG_SETI( 'DL', CCM_DLOW( IDC ) )
            CALL MSG_SETI( 'DH', CCM_DHIGH( IDC ) )
            CALL ERR_REP( 'IRC_FPCO2_ERR2',
     :        'IRC_FPCO2: Detector index ^D is out of bounds [^DL,^DH]',
     :                  STATUS )
            GO TO 999
         END IF
      END DO

*  If either of the input sky coordinates are bad, set all output
*  focal plane coordinates values bad, and return.
      IF( RA .EQ. VAL__BADD .OR. DEC .EQ. VAL__BADD ) THEN
         DO I = 1, NVAL
            ZFP( I ) = VAL__BADR
            YFP( I ) = VAL__BADR
         END DO
         GO TO 999
      END IF

*  Get the boresight positions and scan angles at the requested
*  samples. DPW1 holds sky longitude values, DPW2 holds sky
*  latitude values, DPW3 holds scan angle values, RW4 holds scan speed
*  values.
      CALL IRC1_BPOSI( IDC, NVAL, SAMPLE, DETIND, SCS, DPW1, DPW2, DPW3,
     :                 RW4, STATUS )

*  Convert the given sky position to the same sky coordinate system as
*  that returned by IRC1_BPOSI
      CALL IRA_CONVT( 1, RA, DEC, 'EQUATORIAL(B1950)', SCS,
     :                IRA__IRJEP, A1, B1, STATUS )

      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each sample.
      DO I = 1, NVAL

*  If this sample is bad, set the corresponding elements of arrays
*  ZFP and YFP bad.
         IF( SAMPLE( I ) .EQ. VAL__BADR ) THEN
            ZFP( I ) = VAL__BADR
            YFP( I ) = VAL__BADR

*  Otherwise, resolve the given sky position into components parallel
*  and perpendicular to the focal plane Y axis.
         ELSE
            CALL IRA_DIST2( DPW1( I ), DPW2( I ), DPW3( I ), A1, B1,
     :                      DYFP, DZFP, STATUS )

*  The perpendicular component returned by IRA_DIST2 is in the opposite
*  sense to that of the focal plane. Invert it to get the focal plane Z
*  value.
            ZFP( I ) = -REAL( DZFP )
            YFP( I ) = REAL( DYFP )

         END IF

      END DO

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_FPCO2_ERR3',
     :        'IRC_FPCO2: Unable to convert from sky to focal plane '//
     :        'coordinates. ', STATUS )
      END IF

      END
