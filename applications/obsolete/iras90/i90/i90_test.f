      PROGRAM I90_TEST
*+
*  Name:
*     I90_TEST

*  Purpose:
*     Test installation of the IRAS satellite and mission data
*     subsystem, I90.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Stand-alone program

*  Invocation:
*     RUN I90_TEST

*  Description:
*     This program tests the installation of the I90 subsystem which
*     defines data related to the IRAS satellite and mission.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-JUN-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'I90_DAT'          ! I90_ constants.

*  Global Status:
      INTEGER STATUS             ! Global status

*.

*  Initialise the inherited global status.
      STATUS = SAI__OK

*  Check that the third detector in cross scan order for band 2 is
*  detector #43.
      IF( I90__BDETS( 3, 2 ) .NE. 43 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'D', I90__BDETS( 3, 2 ) )
         CALL ERR_REP( 'I90_TEST_ERR1',
     : 'I90_TEST: Third 25um detector in cross-scan order (#^D) '//
     : 'should be #43.', STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'I90_TEST_ERR2',
     :   'I90_TEST: I90_ installation test failed.', STATUS )

      ELSE
         CALL MSG_OUT( ' ', 'I90_ installation test passed.', STATUS )

      END IF

      END
