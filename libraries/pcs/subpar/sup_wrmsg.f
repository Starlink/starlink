      SUBROUTINE SUBPAR_WRMSG( STRING, STATUS )
*+
*  Name:
*     SUBPAR_WRMSG

*  Purpose:
*     Deliver a message to the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_WRMSG( STRING, STATUS )

*  Description:
*     The given message string is delivered to the user.

*  Arguments:
*     STRING=CHARACTER * ( * ) (given)
*        The text to be delivered.
*     STATUS=INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     If running as a DCL task, then
*        Write the message to the terminal
*     else
*        The inter-task PATH to the task which issued the RUN command is
*        obtained from the SUBPAR common blocks, and the text-string is
*        sent to that task.
*     endif

*  Notes:
*     This subroutine is provided purely for delivering messages
*     from the Message Reporting System (MSG) to the user. It should not
*     be used for any other purpose. If the attempt to deliver the message
*     fails, an error report is made using EMS_REP.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-AUG-1992 (PCTR):
*        Original version based upon SUBPAR_WRITE.
*     27-AUG-1992 (PCTR):
*        Call SUBPAR_WRITE for the time being.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER * ( * ) STRING   ! The text string to be output

*  Status:
      INTEGER STATUS

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call SUBPAR_WRITE.
      CALL SUBPAR_WRITE( STRING, STATUS )

*  Check the status and report an error if necessary.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_MARK
         CALL EMS_REP( 'SUBPAR_MSG_OPTE',
     :                 'SUBPAR: Message delivery failed.', STATUS )
         CALL EMS_RLSE
      END IF

      END
