      SUBROUTINE LPG_TEST( STATUS )
*+
*  Name:
*     LPG_TEST

*  Purpose:
*     Test installation of the  LPG package.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This program tests the installation of the  LPG package. Note, it 
*     is not an exhaustive test of the LPG_ system itself.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL LPG_AGAIN           ! Invoke the application again?
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start up the LPG system, in verbose mode, adding 1 second delay
*  between invocations.
      CALL LPG_START( .TRUE., 1.0, .FALSE., STATUS )

*  Now loop round performing multiple invocations of "application" fred.
      DO WHILE( LPG_AGAIN( STATUS ) ) 
         CALL FRED( STATUS )
      END DO

      END





      SUBROUTINE FRED( STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INDF1, INDF2

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

      CALL NDF_BEGIN

*  Open an existing NDF and copy it to a new file.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )
      CALL LPG_PROP( INDF1, 'DATA', 'OUT', INDF2, STATUS )

      CALL NDF_END( STATUS )

      END
