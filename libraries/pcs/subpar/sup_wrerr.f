      SUBROUTINE SUBPAR_WRERR( STRING, STATUS )
*+
*  Name:
*     SUBPAR_WRERR

*  Purpose:
*     Deliver an error message to the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_WRERR( STRING, STATUS )

*  Description:
*     The given error message string is delivered to the user.

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
*     This subroutine is provided purely for delivering error messages
*     from the Error Reporting System (ERR) to the user. It should not
*     be used for any other purpose. Because this routine is used to
*     deliver reported error messages, it does make an error report
*     (i.e.  using EMS_REP) if it is unsuccessful. However, the status
*     value is returned set to SUBPAR_OPTER, indicating failure.

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
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*) STRING       ! The text string to be output

*  Status:
      INTEGER STATUS

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call SUBPAR_WRITE.
      CALL SUBPAR_WRITE( STRING, STATUS )

      END
