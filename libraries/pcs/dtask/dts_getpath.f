*+  DTASK_GETPATH - get path to task which initiated named action
      SUBROUTINE DTASK_GETPATH ( NAME, PATH, MESSID, STATUS )
*    Description :
*     Obtains the ADAM message path to the task which started the
*     action named along with the associated message number.  The action 
*     must be currently active.
*    Invocation :
*     CALL DTASK_GETPATH ( NAME, PATH, MESSID, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*           action name
*     PATH=INTEGER (returned)
*           path pointer back to controlling task
*     MESSID=INTEGER (given)
*           message number of the OBEY message
*     STATUS=INTEGER
*    Method :
*     Calls dtask_srchlst to obtain pointer to the action in the action
*     list;  then looks up the appropriate path pointer and messid.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVA::ADAM) 22Nov84
*    History :
*     22-NOV-1984  first insertion (REVA::ADAM)
*     16.04.1985:  return messid also (REVAD::BDK)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files (REVAD::BDK)
*     13.10.1992:  add INCLUDE 'PAR_PAR' (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'
      INCLUDE 'DTASK_ERR'
*    Import :
      CHARACTER*(*) NAME      !  name of action
*    Export :
      INTEGER PATH            !  ADAM message path found
      INTEGER MESSID          !  corresponding message number
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'DTASK_CMN'
*    Local variables :
      INTEGER ACTPTR
*-

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
