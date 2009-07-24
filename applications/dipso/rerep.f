      SUBROUTINE REREP( COMM, REP, STATUS )
*+
* Name:
*    REREP

*  Purpose:
*     Re-report an error message.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL REREP( COMM, REP, STATUS )

*  Description:
*     If the current message filtering level is verbose or higher, the supplied
*     report is added to the error stack as a context message. If the
*     filter level is normal or quiet, the current error is annulled and
*     then re-reported using a blank report.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The command name
*     REP = CHARACTER * ( * ) (Given)
*        The report
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-SEP-1994 (DSB):
*        Original version.
*     23-JUL-2009 (TIMJ):
*        Use MSG_FLEVOK rather than MSG_IFLEV
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG__constants
      
*  Arguments Given:
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) REP
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :     OSTAT                 ! The original STATUS value
      LOGICAL
     :     ISVERB                ! Is VERBOSE or DEBUG
*.

*  Return if no error has occurred.
      IF ( STATUS .EQ. SAI__OK ) RETURN

*  Start a new error reporting context, in which the current error
*  status is suppressed. This is done so that the following call to
*  MSGOUT (which follows the usual Starlnk inherited status policy of
*  returning without action if STATUS is set on entry) will not return
*  without action.n.
      CALL ERR_BEGIN( STATUS )

*  Issue the supplied message.
      CALL MSGOUT( COMM, REP, .TRUE., STATUS )

*  Check message reporting level
      ISVERB = MSG_FLEVOK( MSG__VERB, STATUS )
      
*  Reinstate the original error context with its active error status.
      CALL ERR_END( STATUS )      

*  If the current filter level is verbose, return as we are (i.e. leave
*  the current error report stack in tact).  For normal or quiet
*  filtering, save the current status value, annul the error and then
*  re-report it using a blank error report.
      IF( .NOT. ISVERB ) THEN
         OSTAT = STATUS
         CALL ERR_ANNUL( STATUS )      
         STATUS = OSTAT
         CALL ERR_REP( 'REREP_ERR1', ' ', STATUS )            
      END IF            
      
      END
