*+  DTASK_SET - routine handling task set
      SUBROUTINE DTASK_SET ( PATH, NAME, VALUE, MESSID, STATUS )
*    Description :
*     Sets the named task parameter to the given value. 
*     Sends acknowledgment to initiating task.
*    Invocation :
*     CALL DTASK_SET ( PATH, NAME, VALUE, MESSID, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*           path to initiating task
*     NAME=CHARACTER*(*) (given)
*           name of parameter to be set. In a monolith this must be
*           ACTION_KEYWORD:PARAMETER_NAME
*     VALUE=CHARACTER*(*) (given)
*           value to be set
*     MESSID=INTEGER (given)
*           message identifier
*     STATUS=INTEGER
*    Method :
*     Calls the ADAM parameter system primitives, so that the user is 
*     not prompted for an HDS structure to put the parameter value in.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 01May84
*    History :
*     date:  changes (institution::username)
*     8-MAY-1984  first insertion (REVAD::JAC)
*     11-MAY-1984  test of autodoc (REVAD::JAC)
*     19-JUN-1984  changed from "put"; added acknowledgment (REVA::ADAM)
*     19-JUN-1984  repaired msg_value to msg_val ! (REVA::ADAM)
*     02-NOV-1984  use full parameter system (REVAD::BDK)
*     16-NOV-1984  new version with parameter system (REVA::ADAM)
*     11-JUN-1987:  handle monoliths (REVAD::BDK)
*     15-JAN-1991:  check if task is monolith (RLVAD::AJC)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files (REVAD::BDK)
*     09.05.1991:  clear-out ERR and MSG systems (REVAD::BDK)
*     13.05.1991:  use COMSHUT (REVAD::BDK)
*     04.06.1991:  Remove reference to DDMSG (ROE::BMC)
*     04.06.1991:  Correct call to SUBPAR_FINDPAR to use NAMECODE when
*                  generating an error (ROE::BMC)
*     07.06.1991:  insist COLPOS>1 for a monolith (REVAD::BDK)
*     08.03.1993:  remove include MESSYS_PAR (RLVAD::AJC)
*     24.05.1995:  Report on no action for monolith (RLVAD::AJC)
*     04.08.1995:  Allow non-monoliths to have action:name form (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'ADAM_DEFNS'

*    Import :
      INTEGER PATH         !  path to initiating task
      CHARACTER NAME*(*)   !  name of parameter to be set
      CHARACTER VALUE*(*)  !  value it is to be set to
      INTEGER MESSID       !  message identifier

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER NAMECODE     ! code number of parameter
      INTEGER COLPOS       ! position of ':' in NAME
      INTEGER ACODE        ! action pointer (returned by FINDACT
                           ! but not used)
      LOGICAL MONO         ! if the task is a monolith
      INTEGER MESSTATUS    ! status returned to initiating task
*-

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   If this is a monolith, the name must include the keyword of the 
*   relevant action within the monolith in the form KEY:PARNAME.
*   If not a monolith this form is optional, any KEY part will be ignored.
*
*   Find any task/name separator ':'.
*   COLPOS will be 0 if there isn't one.
*
      COLPOS = INDEX ( NAME, ':' )

      CALL SUBPAR_MLITH ( MONO, STATUS )
      IF ( MONO ) THEN
*
*      It is a monolith 
*
         IF ( COLPOS .GT. 1 ) THEN
*
*         Set-up the parameter system for the action.
*
            CALL SUBPAR_FINDACT ( NAME(1:COLPOS-1), ACODE, STATUS )

         ELSE
*
*         No action component in parameter specification
*
            STATUS = DTASK__ACTPAR
            NAMECODE = 0
            CALL ERR_REP ( ' ', 'DTASK: ' //
     :      'SET parameter not of form "task:parameter" for a monolith',
     :       STATUS )

         ENDIF

      ENDIF
*
*   Look-up the named parameter
*
      CALL SUBPAR_FINDPAR ( NAME(COLPOS+1:), NAMECODE, STATUS )
*
*   Put the value into it
*
      CALL SUBPAR_CMDPAR ( NAMECODE, VALUE, STATUS )
*
*   Acknowledge
*
      MESSTATUS = STATUS
      STATUS = SAI__OK
      CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, SET, NAME, VALUE, 
     :  STATUS )

      END
