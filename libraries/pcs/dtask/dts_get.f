*+  DTASK_GET - routine handling d-task get
      SUBROUTINE DTASK_GET ( PATH, NAME, MESSID, STATUS )
*    Description :
*     Obtains the value of the named d-task parameter.  If the parameter
*     does not exist, an appropriate status is returned.
*     Sends acknowledgment containing the parameter value to the 
*     requesting task.
*    Invocation :
*     CALL DTASK_GET ( PATH, NAME, MESSID, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*           path to requesting task
*     NAME=CHARACTER*(*) (given)
*           name of parameter to be got
*     MESSID=INTEGER (given)
*           message id
*     STATUS=INTEGER
*    Method :
*     Stop the parameter system prompting by disabling its 
*     communications. Ask the parameter system for the value as a 
*     character string scalar, then re-enable the communications. 
*     Finally, close the transaction.
*    Deficiencies :
*     Disabling and enabling communications is a frig to allow PAR_GET0C 
*     to be used. The parameter system does not provide a call to carry 
*     out the GET context properly.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 01May84
*     Dennis Kelly (REVAD::BDK)
*     Alan Chipperfield (STARLINK)
*    History :
*     date:  changes (institution::username)
*     8-MAY-1984  first insertion (REVAD::JAC)
*     19-JUN-1984  added acknowledgment (REVA::ADAM)
*     16-NOV-1984  update documentation - new parameter system (REVA::ADAM)
*     17-JAN-1990  increase VALUE 80 -> 132 characters (RLVAD::AJC)
*     15-JAN-1991  handle monoliths - checking if task is
*                  monolith 
*                  Also clear value initially (RLVAD::AJC)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  use MESSYS__MESSAGE (REVAD;;BDK)
*     09.05.1991:  flush ERR and MSG systems (REVAD::BDK)
*     13.05.1991:  use COMSHUT (REVAD::BDK)
*     04.06.1991:  Disable prompting on the PAR_GET0C (ROE::BMC)
*     07.06.1991:  change comments and status handling (REVAD::BDK)
*     08.03.1993:  Use MESSYS__VAL_LEN - remove include DDMSG (RLVAD::AJC)
*     27.07.1993:  Use SUBPAR not PAR_GET0C (RLVAD::AJC)
*     11.02.1994:  Return only used length of VAL (RLVAD::AJC)
*     17.08.1994:  Call new SUBPAR_GET to get values of all types (RLVAD::AJC)
*     24.05.1995:  Report on no action for monolith (RLVAD::AJC)
*     04.08.1995:  Allow non-monoliths to have action:name form (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'MESSYS_PAR'
*    Import :
      INTEGER PATH                  ! path to requesting task
      CHARACTER*(*) NAME            ! name of parameter to be got
      INTEGER MESSID                ! message id
*    Status :
      INTEGER STATUS
*    External routines:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN
*    Local variables :
      INTEGER COLPOS                ! position of ':' in NAME
      INTEGER ACODE                 ! action pointer (returned by FINDACT 
                                    ! but not used)
      LOGICAL MONO                  ! if the task is a monolith
      CHARACTER*(MESSYS__VAL_LEN) VALUE ! value obtained
      INTEGER VALLEN                ! used length of value
      INTEGER MESSTATUS             ! status returned to requesting task
      INTEGER NAMECODE              ! code number of parameter
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
         IF ( COLPOS .NE. 0 ) THEN
*
*         Set-up the parameter system for the task name
*
            CALL SUBPAR_FINDACT ( NAME(1:COLPOS-1), ACODE, STATUS )

         ELSE
*
*         No action component in parameter specification
*
            STATUS = DTASK__ACTPAR
            NAMECODE = 0
            CALL ERR_REP ( ' ', 'DTASK: ' //
     :      'GET parameter not of form "task:parameter" for a monolith',
     :       STATUS )

         ENDIF

      ENDIF
*
*   Get parameter value - initialise to blank to cover failure.
*

** frig
** to prevent prompting
      VALUE = ' '
      CALL SUBPAR_PUTPATH ( 0, 0, STATUS )
      CALL SUBPAR_FINDPAR ( NAME(COLPOS+1:), NAMECODE, STATUS )
      CALL SUBPAR_GET ( NAMECODE, VALUE, STATUS )
      MESSTATUS = STATUS
      STATUS = SAI__OK
      CALL SUBPAR_PUTPATH ( PATH, MESSID, STATUS )
** endfrig

*
*   Acknowledge.
*
      STATUS = SAI__OK
      VALLEN = MAX( 1, CHR_LEN( VALUE ) )
      CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, GET, NAME,
     : VALUE(1:VALLEN), STATUS )

      END
