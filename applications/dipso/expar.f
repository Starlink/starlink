      SUBROUTINE EXPAR( COMM, PARAMS, MAXPAR, STATUS )
*+
* Name:
*    EXPAR

*  Purpose:
*     Check for redundant command line parameter values

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL EXPAR( COMM, PARAMS, MAXPAR, STATUS )

*  Description:
*     If the given PARAMS string contains more than MAXPAR words, a
*     warning is given saying that redundant parameter values will be
*     ignored.

*  Arguments:
*     COMM= CHARACTER * ( * ) (Given)
*        The command name.
*     PARAMS = CHARACTER * ( * ) (Given)
*        A string holding all the command line parameters supplied by
*        the user.
*     MAXPAR = INTEGER (Given)
*        The maximum number of command line parameters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-SEP-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) PARAMS
      INTEGER MAXPAR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        W1*50,             ! First redundant parameter value
     :        W2*50              ! Second redundant parameter value

      INTEGER
     :        W1LEN,             ! Used length of W1
     :        W2LEN              ! Used length of W2

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to find a (MAXPAR+1)'th word in the supplied string.
      CALL FWORD( PARAMS, MAXPAR+1, W1, W1LEN )

*  If found, try to find a (MAXPAR+2)'th word in the supplied string.
      IF( W1LEN .GT. 0 ) THEN
         CALL FWORD( PARAMS, MAXPAR+2, W2, W2LEN )

*  Issue a suitable warning message.
         IF( W2LEN .GT. 0 ) THEN
            CALL MSG_SETC( 'P', W1 )
            CALL MSGOUT( COMM, 'Extra parameters ignored ''^P...''',
     :                   .TRUE., STATUS )

         ELSE
            CALL MSG_SETC( 'P', W1 )
            CALL MSGOUT( COMM, 'Extra parameter ignored ''^P''',
     :                   .TRUE., STATUS )

         END IF

      END IF

      END
