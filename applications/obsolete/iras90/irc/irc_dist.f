      SUBROUTINE IRC_DIST( IDC, SAMP1, DETIN1, SAMP2, DETIN2, DIST,
     :                     STATUS )
*+
*  Name:
*     IRC_DIST

*  Purpose:
*     Find the in-scan distance between two samples.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_DIST( IDC, SAMP1, DETIN1, SAMP2, DETIN2, DIST, STATUS )

*  Description:
*     The in-scan distance from the sample specified by SAMP1 and DETIN1
*     to the sample specified by SAMP2 and DETIN2 is found. Positive
*     values imply that the displacement from the first sample to the
*     second sample is in the direction of the positive focal plane
*     Y axis (see ID1 Appendix E).

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     SAMP1 = REAL (Given)
*        The first fractional sample number. If this has the Starlink
*        "BAD" value (VAL__BADR) then DIST is returned set to the bad
*        value.
*     DETIN1 = INTEGER (Given)
*        The detector index to which SAMP1 refers.
*     SAMP2 = REAL (Given)
*        The second fractional sample number. If this has the Starlink
*        "BAD" value (VAL__BADR) then DIST is returned set to the bad
*        value.
*     DETIN2 = INTEGER (Given)
*        The detector index to which SAMP2 refers.
*     DIST = REAL (Returned)
*        The in-scan distance between the two samples, in radians.
*        Positive if the displacement from SAMP1 to SAMP2 is in the
*        same direction as the positive focal plane Y axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-FEB-1991 (DSB):
*        Original version.
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
      INCLUDE 'IRC_ERR'          ! IRC errors.

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
      REAL    SAMP1
      INTEGER DETIN1
      REAL    SAMP2
      INTEGER DETIN2

*  Arguments Returned:
      REAL    DIST

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
         CALL ERR_REP( 'IRC_DIST_ERR1',
     :                 'IRC_DIST: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that the detector indices are within the bounds of the NDF
*  second dimension.
      IF( DETIN1 .LT. CCM_DLOW( IDC ) .OR.
     :    DETIN1 .GT. CCM_DHIGH( IDC ) ) THEN
         STATUS = IRC__BADDI
         CALL MSG_SETI( 'D', DETIN1 )
         CALL MSG_SETI( 'DL', CCM_DLOW( IDC ) )
         CALL MSG_SETI( 'DH', CCM_DHIGH( IDC ) )
         CALL ERR_REP( 'IRC_DIST_ERR2',
     :        'IRC_DIST: Detector index ^D (DETIN1) is '//
     :        'out of bounds [^DL,^DH]', STATUS )
         GO TO 999
      END IF

      IF( DETIN2 .LT. CCM_DLOW( IDC ) .OR.
     :    DETIN2 .GT. CCM_DHIGH( IDC ) ) THEN
         STATUS = IRC__BADDI
         CALL MSG_SETI( 'D', DETIN2 )
         CALL MSG_SETI( 'DL', CCM_DLOW( IDC ) )
         CALL MSG_SETI( 'DH', CCM_DHIGH( IDC ) )
         CALL ERR_REP( 'IRC_DIST_ERR3',
     :        'IRC_DIST: Detector index ^D (DETIN2) is '//
     :        'out of bounds [^DL,^DH]', STATUS )
         GO TO 999
      END IF

*  If both sample number are good, call an internal routine to do the
*  work.
      IF( SAMP1 .NE. VAL__BADR .AND. SAMP2 .NE. VAL__BADR ) THEN
         CALL IRC1_DISTI( IDC, SAMP1, DETIN1, SAMP2, DETIN2, DIST,
     :                    STATUS )

*  Otherwise set DIST to the bad value.
      ELSE
         DIST = VAL__BADR

      END IF

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_DIST_ERR4',
     :        'IRC_DIST: Unable to find distance between two samples ',
     :                 STATUS )
      END IF


      END
