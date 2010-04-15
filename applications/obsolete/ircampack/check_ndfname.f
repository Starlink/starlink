      SUBROUTINE CHECK_NDFNAME( STATUS )
*+
*  Name:
*     CHECK_NDFNAME

*  Purpose:
*     Check a specified NDF exists and report its full name.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHECK_NDFNAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The name of an NDF is obtained from the environment, using
*     a parameter named "NDF". If the NDF does not exist then a
*     field of six plus-signs is displayed. Otherwise, the full
*     name of the NDF is displayed.

*  Usage:
*     CHECK_NDFNAME NDF

*  ADAM Parameters:
*     NDF = NDF (Read)
*        The NDF to be checked.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INDF               ! The NDF identifier

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Attempt to access the NDF.
      CALL NDF_EXIST( 'NDF', 'READ', INDF, STATUS )

*  If an NDF was found, assign its full name to a message token, and
*  annul the NDF identifier.
      IF( INDF .NE. NDF__NOID ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL NDF_ANNUL( INDF, STATUS )

*  Otherwise, assign a field of plus-signs to the message token.
      ELSE
         CALL MSG_SETC( 'NDF', '++++++' )

      END IF

*  Display the message. Give it a priority of MSG__QUIET so that it will
*  always be displayed.
      CALL MSG_OUTIF( MSG__QUIET, 'CHECK_NDFNAME_MSG1', '^NDF', STATUS )

      END
