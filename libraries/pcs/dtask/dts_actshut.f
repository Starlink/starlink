*+  DTASK_ACTSHUT - shut-down an action
      SUBROUTINE DTASK_ACTSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, 
     :  ACTPTR, ANAME, AKEY, VALUE, STATUS )
*    Description :
*     Shut-down an action, including sending the final acknowledgement.
*    Invocation :
*     CALL DTASK_ACTSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, 
*    :  ACTPTR, ANAME, AKEY, VALUE, STATUS )
*    Parameters :
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
*    Method :
*     Tell the TASK library to forget about this action. Tell the DTASK 
*     library the action is no longer active. Flush the ERR and MSG 
*     systems. Send the final acknowledgment.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     13.05.1991:  original (REVAD::BDK)
*     27.05.1991:  use ERR_CLEAR (REVAD::BDK)
*     11.06.1991:  use DTASK_COMSHUT (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
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

*    Status :
      INTEGER STATUS

*-

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
