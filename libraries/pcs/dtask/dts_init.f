*+  DTASK_INIT - initialise the main libraries
      SUBROUTINE DTASK_INIT ( NAME, NLENGTH, STATUS )
*    Description :
*     Initialises the main ADAM libraries.
*    Invocation :
*     CALL DTASK_INIT ( NAME, NLENGTH; STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*           name of this task
*     NLENGTH=INTEGER (given)
*           length of task name
*     STATUS=INTEGER
*    Method :
*     Initialise into the ADAM message system (MESSYS).
*     Initialise the parameter system (SUBPAR).
*     Initialise the task support library (TASK).
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 22May84
*    History :
*     date:  changes (institution::username)
*     22-MAY-1984  first insertion (REVA::ADAM])
*     22-MAY-1984  initialise action list common block (REVA::ADAM])
*     19-JUN-1984  change 'NORMAL' to 'ADAM__OK' (REVA::ADAM)
*     21-JUN-1984  add interrupt flag (REVA::ADAM)
*     26-OCT-1984  add NLENGTH to argument list and initialise parameter 
*                  system (REVAD::BDK)
*     16-NOV-1984  new version with parameter system (REVA::ADAM)
*     30-APR-1989  add call to TASK_INIT_MESSINFO to initialise list of
*                  active actions in subsidiary tasks (AAOEPP::WFL)
*     30-APR-1989  add call to DTASK_SETDUMP (AAOEPP::WFL)
*     01-MAR-1990  remove call to DTASK_SETDUMP (AAOEPP::WFL)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files (REVAD::BDK)
*     09.05.1991:  initialise ERR and MSG (REVAD::BDK
*     15.05.1991:  remove initialise INTRUPT_FLAG (RLVAD::AJC)
*     28.05.1991:  remove initialise ERR and MSG (REVAD::BDK)
*     07.06.1991:  change comments (REVAD::BDK)
*     13.10.1992:  add INCLUDE 'PAR_PAR' (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*      4.08.1994:  Pass used part of NAME to MESSYS  (RLVAD::AJC)
*     29.09.1994:  Force DTASK_GETPATH to be loaded  (RLVAD::AJC)
*     11.06.2001:  Call AMS (FAMS) directly (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'
*    Import :
      CHARACTER*(*) NAME   !  name of this task
      INTEGER NLENGTH      !  length of task name
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'DTASK_CMN'
*    Local variables :
      INTEGER PATH         !  unused argument for DTASK_GETPATH
      INTEGER MESSID       !  unused argument for DTASK_GETPATH
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Initialise into message system.
*
      CALL FAMS_INIT( NAME(1:NLENGTH), STATUS )
*
*   Initialise action list common block.
*
      NACTS = 0
*
*   Initialise parameter system.
*
      CALL SUBPAR_ACTIV ( NAME, NLENGTH, STATUS )
*
*   Initialise the list of active actions in subsidiary tasks.
*
      CALL TASK_INIT_MESSINFO ( STATUS )
*
*   Put a dummy call to DTASK_GETPATH to force it to be loaded
*   in case TASK_TRIGGER needs it
*  
      IF ( STATUS .NE. SAI__OK )
     :   CALL DTASK_GETPATH( NAME, PATH, MESSID, STATUS )

      END
