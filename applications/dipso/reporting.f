      SUBROUTINE REPORTING( COMM, PARAMS, STATUS )
*+
*  Name:
*     REPORTING

*  Purpose:
*     Implements the REPORTING command.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL REPORTING( COMM, PARAMS, STATUS )

*  Description:
*     The REPORTING command sets the MSG message filtering level.  The
*     default behaviour is not to change the message filtering level.
*     Numerical values (0,1,2) are used to represent the three possible
*     reporting levels rather than strings ("quiet","normal" and
*     "verbose") because the normal level in DIPSO (i.e. the default
*     used until the user establishes a new level using thism command)
*     is actually "verbose" and NOT "normal". To avoid confusion, we
*     use integers when communicating with the user!

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The name of the command which invoked this routine. This will
*        usually be "REPORTING".
*     PARAMS = CHARACTER * ( * ) (Given)
*        Any text supplied by the user on the command line following the
*        command name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER PARAMS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variable:
      INTEGER
     :        DEFIND,            ! Default value
     :        FILTER,            ! Current filter level
     :        INDEX              ! Requested value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give a warning if two or more parameters were supplied.
      CALL EXPAR( COMM, PARAMS, 1, STATUS )

*  Set up an integer value representing the current filter level. This
*  will be used as the default when prompting the user.
      CALL MSG_IFLEV( FILTER, ' ', STATUS )
      IF( FILTER .EQ. MSG__QUIET ) THEN
         DEFIND = 0

      ELSE IF( FILTER .EQ. MSG__NORM ) THEN
         DEFIND = 1

      ELSE
         DEFIND = 2

      END IF

*  Get the integer value which specifies the required message filtering
*  level.
      CALL GET0I( PARAMS, 1, .FALSE., COMM, 'Message filtering level',
     :            DEFIND, INDEX, STATUS )

*  Tell the user what is about to happen and then set the message
*  filtering level.
      IF( INDEX .LE. 0 ) THEN
         CALL MSGOUT( COMM, 'Setting the message filtering level to 0',
     :                .FALSE., STATUS )
         CALL MSG_IFSET( MSG__QUIET, STATUS )

      ELSE IF( INDEX .EQ. 1 ) THEN
         CALL MSGOUT( COMM, 'Setting the message filtering level to 1',
     :                .FALSE., STATUS )
         CALL MSG_IFSET( MSG__NORM, STATUS )

      ELSE
         CALL MSGOUT( COMM, 'Setting the message filtering level to 2',
     :                .FALSE., STATUS )
         CALL MSG_IFSET( MSG__VERB, STATUS )

      END IF

*  If an error has occurred, give a context warning message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )
         CALL MSGOUT( COMM, 'An error occurred while setting the '//
     :                'message reporting level.', .TRUE., STATUS )
         CALL ERR_END( STATUS )
      END IF

      END
