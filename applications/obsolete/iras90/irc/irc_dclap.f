      SUBROUTINE IRC_DCLAP( IDC, DETIND, RA, DEC, CLSAMP, CLZFP,
     :                      STATUS )
*+
*  Name:
*     IRC_DCLAP

*  Purpose:
*     Find the position of closest approach of a detector track to a
*     given sky position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_DCLAP( IDC, DETIND, RA, DEC, CLSAMP, CLZFP, STATUS )

*  Description:
*     The path of a detector centre as it passes over the sky is called
*     the "detector track". This routine finds the fractional sample
*     number (from the row specified by the given detector index) at
*     which the closest approach of the detector to a given sky
*     position is reached.  The focal plane z coordinate of the given
*     position, at the moment corresponding to the closest approach is
*     also returned.  If the position of closest approach lies outside
*     the bounds of the first dimension of the NDF, then the sample
*     number returned represents an extrapolated position. If it is not
*     possible to produce a reliable extrapolated sample number, then
*     STATUS is returned with the value IRC__BADEX, and an error report
*     is made.  It is then the responsibility of the calling routine to
*     handle the situation.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     DETIND = INTEGER (Given)
*        The detector index. The returned sample number (CLSAMP) refers
*        to this detector.
*     RA = DOUBLE PRECISION (Given)
*        The Right Ascension (B1950, FK4) of the given sky position.
*        If this has the Starlink "BAD" value (VAL__BADD) then CLSAMP
*        and CLZFP are returned with the bad value.
*     DEC = DOUBLE PRECISION (Given)
*        The Declination (B1950, FK4) of the given sky position.
*        If this has the Starlink "BAD" value (VAL__BADD) then CLSAMP
*        and CLZFP are returned with the bad value.
*     CLSAMP = REAL (Returned)
*        The fractional sample number at which the point of closest
*        approach of the detector track to the given sky position is
*        reached. Accurate to about a tenth of a sample.
*     CLZFP = REAL (Returned)
*        The focal plane Z coordinate of the given position at the point
*        of closest approach. In radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-FEB-1991 (DSB):
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
      INTEGER DETIND
      DOUBLE PRECISION RA
      DOUBLE PRECISION DEC

*  Arguments Returned:
      REAL    CLSAMP
      REAL    CLZFP

*  Status:
      INTEGER STATUS             ! Global status
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
         CALL ERR_REP( 'IRC_DCLAP_ERR1',
     :                 'IRC_DCLAP: Invalid IRC identifier supplied',
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
         CALL ERR_REP( 'IRC_DCLAP_ERR2',
     :        'IRC_DCLAP: Detector index ^D is out of bounds [^DL,^DH]',
     :                  STATUS )
         GO TO 999
      END IF

*  Check that both given coordinates are valid...
      IF( RA .NE. VAL__BADD .AND. DEC .NE. VAL__BADD ) THEN

*  If so, call a lower level routine to do the work.
         CALL IRC1_DCLPI( IDC, DETIND, RA, DEC, CLSAMP, CLZFP, STATUS )

*  Otherwise, set CLSAMP and CLZFP to the bad value.
      ELSE
         CLSAMP = VAL__BADR
         CLZFP = VAL__BADR

      END IF

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_DCLAP_ERR3',
     :   'IRC_DCLAP: Unable to find closest approach of a detector '//
     :   'to a given position', STATUS )
      END IF

      END
