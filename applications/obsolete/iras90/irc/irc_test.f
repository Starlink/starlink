      SUBROUTINE IRC_TEST( STATUS )
*+
*  Name:
*     IRC_TEST

*  Purpose:
*     Test installation of the IRAS90 CRDD handling package, IRC.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IRC_TEST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program tests the installation of the IRAS90 CRDD handling
*     package (IRC_). Note, it is not an exhaustive test of the
*     IRC_ system itself.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-MAY-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! I90_ data values.
      INCLUDE 'IRC_PAR'          ! IRC_ constants.
      INCLUDE 'IRC_ERR'          ! IRC_ error constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION ANGLE     ! Position angle of detector track at
                                 ! point of closest approach of
                                 ! detector with index 1 to the
                                 ! reference point.
      INTEGER BAND               ! IRAS band no. of data in CRDD file.
      REAL CLZFP                 ! Focal plane Z at point of closest
                                 ! approach of detector with index 1
                                 ! to the reference point.
      REAL CLSAMP                ! Sample no. at point of closest
                                 ! approach of detector with index 1
                                 ! to the reference point.
      DOUBLE PRECISION DEC       ! DEC of point of closest approach of
                                 ! detector with index 1 to the
                                 ! reference point.
      INTEGER IDC                ! IRC identifier for CRDD file.
      INTEGER INDF               ! NDF identifier for CRDD file.
      REAL NOMSPD                ! Nominal scan speed of CRDD file.
      INTEGER OBS                ! OBS of data in CRDD file.
      DOUBLE PRECISION RA        ! RA of point of closest approach of
                                 ! detector with index 1 to the
                                 ! reference point.
      DOUBLE PRECISION REFDEC    ! DEC of CRDD reference point.
      DOUBLE PRECISION REFRA     ! RA of CRDD reference point.
      INTEGER SOP                ! SOP of data in CRDD file.
      REAL SPEED                 ! Scan speed at point of closest
                                 ! approach of detector with index 1
                                 ! to the reference point.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an NDF identifier for the CRDD file.
      CALL NDF_ASSOC( 'IN', 'READ', INDF, STATUS )

*  Initialise the IRC_ system.
      CALL IRC_INIT( STATUS)

*  Import the CRDD file into the IRC system.
      CALL IRC_IMPRT( INDF, IDC, STATUS )

*  Get the global information from the CRDD file.
      CALL IRC_INFO( IDC, BAND, REFRA, REFDEC, NOMSPD, SOP, OBS,
     :               STATUS )

*  Check band number, SOP and OBS. If any item is not correct, report
*  an error.
      IF( BAND .NE. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'BAND', BAND )
         CALL ERR_REP( 'IRC_TEST_ERR1',
     : 'IRC_TEST: Band no. is ^BAND, should be 1', STATUS )
      END IF

      IF( SOP .NE. 382 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'SOP', SOP )
         CALL ERR_REP( 'IRC_TEST_ERR2',
     : 'IRC_TEST: SOP no. is ^SOP, should be 382', STATUS )
      END IF

      IF( OBS .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'OBS', OBS )
         CALL ERR_REP( 'IRC_TEST_ERR3',
     : 'IRC_TEST: Band no. is ^OBS, should be 0', STATUS )
      END IF

*  Find the closest approach of the detector with index 1 to the
*  reference point.
      CALL IRC_DCLAP( IDC, 1, REFRA, REFDEC, CLSAMP, CLZFP, STATUS )

*  Find the actual RA and DEC of the detector at the closest approach.
      CALL IRC_DPOS( IDC, 1, CLSAMP, 1, RA, DEC, ANGLE, SPEED, STATUS )

*  If either the RA or the DEC is wrong, report an error.
      IF( ABS( RA - 3.4932870310024 ) .GT. 7.0D-6 .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'RA', RA )
         CALL ERR_REP( 'IRC_TEST_ERR4',
     : 'IRC_TEST: RA of closest approach is ^RA; should be '//
     : '3.4932870310024 ', STATUS )
      END IF

      IF( ABS( DEC + 0.74296235582566 ) .GT. 5.0D-7 .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'DEC', DEC )
         CALL ERR_REP( 'IRC_TEST_ERR5',
     : 'IRC_TEST: DEC of closest approach is ^DEC; should be '//
     : '-0.74296235582566', STATUS )
      END IF

*  Release the CRDD file from the IRC and NDF systems.
      CALL IRC_ANNUL( IDC, STATUS )
      CALL NDF_ANNUL( INDF, STATUS )

*  Close down the IRC system.
      CALL IRC_CLOSE( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_TEST_ERR6',
     :   'IRC_TEST: IRC_ installation test failed.', STATUS )

      ELSE
         CALL MSG_OUT( ' ', 'IRC_ installation test passed.', STATUS )

      END IF

      END
