      SUBROUTINE IRC_OFFST( IDC, SAMP1, DETIN1, DETIN2, DIST, SAMP2,
     :                      STATUS )
*+
*  Name:
*     IRC_OFFST

*  Purpose:
*     Find a sample which is a given in-scan distance away from a given
*     sample.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_OFFST( IDC, SAMP1, DETIN1, DETIN2, DIST, SAMP2, STATUS )

*  Description:
*     The calling routine specifies a detector index and sample number
*     (DETIN1 and SAMP1) to define a starting point, and a distance
*     (DIST) by which to move away from the starting point in the focal
*     plane Y direction (see ID1 appendix E). Firstly, the sample
*     number (from the same detector) is found which is the given
*     distance away from the starting point. If DETIN2 is the same as
*     DETIN1, this sample number is returned in SAMP2. If not, the
*     sample number from detector specified by DETIN2 is returned at
*     which that detector reaches the same in-scan position.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     SAMP1 = REAL (Given)
*        A fractional sample number which gives the starting point of
*        the offset operation. If SAMP1 has the Starlink "BAD" value
*        (VAL__BADR) then SAMP2 is returned bad.
*     DETIN1 = INTEGER (Given)
*        The detector index to which SAMP1 refers.
*     DETIN2 = INTEGER (Given)
*        The detector index to which SAMP2 should refer.
*     DIST = REAL (Given)
*        The arc-distance to offset away from SAMP1, in radians. A
*        positive offset moves in the focal plane Y direction. If
*        DIST has the Starlink "BAD" value (VAL__BADR) then SAMP2 is
*        returned bad.
*     SAMP2 = REAL (Returned)
*        The fractional sample number from detector DETIN2 which is the
*        required in-scan distance away from SAMP1 from detector
*        DETIN1.
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
      INTEGER DETIN2
      REAL    DIST

*  Arguments Returned:
      REAL    SAMP2

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
         CALL ERR_REP( 'IRC_OFFST_ERR1',
     :                 'IRC_OFFST: Invalid IRC identifier supplied',
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
         CALL ERR_REP( 'IRC_OFFST_ERR2',
     :                 'IRC_OFFST: Detector index ^D (DETIN1) is '//
     :                 'out of bounds [^DL,^DH]', STATUS )
         GO TO 999
      END IF

      IF( DETIN2 .LT. CCM_DLOW( IDC ) .OR.
     :    DETIN2 .GT. CCM_DHIGH( IDC ) ) THEN
         STATUS = IRC__BADDI
         CALL MSG_SETI( 'D', DETIN2 )
         CALL MSG_SETI( 'DL', CCM_DLOW( IDC ) )
         CALL MSG_SETI( 'DH', CCM_DHIGH( IDC ) )
         CALL ERR_REP( 'IRC_OFFST_ERR3',
     :                 'IRC_OFFST: Detector index ^D (DETIN2) is '//
     :                 'out of bounds [^DL,^DH]', STATUS )
         GO TO 999
      END IF

*  If DIST and SAMP1 are not bad, call an internal routine to do the
*  work.
      IF( SAMP1 .NE. VAL__BADR .AND. DIST .NE. VAL__BADR ) THEN
         CALL IRC1_OFFSI( IDC, SAMP1, DETIN1, DETIN2, DIST, SAMP2,
     :                    STATUS )

*  Otherwise, set SAMP2 bad.
      ELSE
         SAMP2 = VAL__BADR

      END IF

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_OFFST_ERR4',
     :            'IRC_OFFST: Unable to find an offset sample position',
     :                 STATUS )
      END IF


      END
