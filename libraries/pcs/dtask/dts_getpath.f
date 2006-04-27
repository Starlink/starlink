      SUBROUTINE DTASK_GETPATH ( NAME, PATH, MESSID, STATUS )
*+
*  Name:
*     DTASK_GETPATH

*  Purpose:
*     Get path to task which initiated named action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_GETPATH ( NAME, PATH, MESSID, STATUS )

*  Description:
*     Obtains the ADAM message path to the task which started the
*     action named along with the associated message number.  The action 
*     must be currently active.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*           action name
*     PATH=INTEGER (returned)
*           path pointer back to controlling task
*     MESSID=INTEGER (given)
*           message number of the OBEY message
*     STATUS=INTEGER

*  Algorithm:
*     Calls dtask_srchlst to obtain pointer to the action in the action
*     list;  then looks up the appropriate path pointer and messid.

*  Authors:
*     John Cooke (REVA::ADAM) 22Nov84
*     {enter_new_authors_here}

*  History:
*     22-NOV-1984 (REVA::ADAM):
*        First insertion
*     16-APR-1985 (REVAD::BDK):
*        Return messid also
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     13-OCT-1992 (RLVAD::AJC):
*        Add INCLUDE 'PAR_PAR'
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'
      INCLUDE 'DTASK_ERR'
*  Arguments Given:
      CHARACTER*(*) NAME      !  name of action
*  Arguments Returned:
      INTEGER PATH            !  ADAM message path found
      INTEGER MESSID          !  corresponding message number
*  Status:
      INTEGER STATUS
*  Global Variables:
      INCLUDE 'DTASK_CMN'
*  Local Variables:
      INTEGER ACTPTR
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   search the action list ...
      CALL DTASK_SRCHLST ( NAME, ACTPTR, STATUS )

      IF ( STATUS .EQ. DTASK__ACTACTIVE ) THEN
*      it is on the action list ...
         STATUS = SAI__OK
         PATH = ACTPATH ( ACTPTR )
         MESSID = ACTMESSID ( ACTPTR )

      ELSE
*      return the status ...
         CONTINUE

      ENDIF

      END
