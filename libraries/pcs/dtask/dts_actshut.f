      SUBROUTINE DTASK_ACTSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, 
     :  ACTPTR, ANAME, AKEY, VALUE, STATUS )
*+
*  Name:
*     DTASK_ACTSHUT

*  Purpose:
*     Shut-down an action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_ACTSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, 
*     :  ACTPTR, ANAME, AKEY, VALUE, STATUS )

*  Description:
*     Shut-down an action, including sending the final acknowledgement.

*  Arguments:
*     PATH=INTEGER (given)
*           message path needed for reply
*     MESSID=INTEGER given)
*           transaction number needed for reply
*     MESSTATUS=INTEGER ( given)
*           status to be returned in completion message
*     CONTEXT=INTEGER (given)
*           context to be returned in completion message
*     ACTPTR=INTEGER (given)
*           action pointer
*     ANAME=CHARACTER*(*) (given)
*           action name
*     AKEY=CHARACTER*(*) (given)
*           action keyword
*     VALUE=CHARACTER*(*) (given and returned)
*           string to be returned in completion message
*     STATUS=INTEGER

*  Algorithm:
*     Tell the TASK library to forget about this action. Tell the DTASK 
*     library the action is no longer active. Flush the ERR and MSG 
*     systems. Send the final acknowledgment.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     13-MAY-1991 (REVAD::BDK):
*        Original
*     27-MAY-1991 (REVAD::BDK):
*        Use ERR_CLEAR
*     11-JUN-1991 (REVAD::BDK):
*        Use DTASK_COMSHUT
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER PATH               ! message path needed for reply
      INTEGER MESSID             ! transaction number needed for reply 
      INTEGER MESSTATUS          ! status to be returned in completion
                                 ! message 
      INTEGER CONTEXT            ! context to be returned in completion
                                 ! message 
      INTEGER ACTPTR             ! action pointer
      CHARACTER*(*) ANAME        ! action name
      CHARACTER*(*) AKEY         ! keyword of action required
      CHARACTER*(*) VALUE        ! command line parameter string

*  Status:
      INTEGER STATUS

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Shut down the action
*
      CALL TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )
      CALL DTASK_REMLST ( ANAME, STATUS )
*
*   Close communications
*
      CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, AKEY, 
     :  VALUE, STATUS )

      END
