*+  ADAM_PATH - obtain path pointer to another task
      SUBROUTINE ADAM_PATH ( TASK_NAME, PATH, STATUS )
*    Description :
*     Returns a pointer for intertask communication to the task 
*     described by TASK_NAME.
*    Invocation :
*     CALL ADAM_PATH ( TASK_NAME, PATH, STATUS )
*    Parameters :
*     TASK_NAME=CHARACTER*(*) (given)
*           name of task to which path is required
*     PATH=INTEGER (returned)
*           pointer to the path
*    Method :
*     Call MESSYS_PATH. If this succeeds it will return a status with 
*     value ADAM__OK if a new path to the task has been set up, or 
*     MESSYS_PATHOPEN if the path already existed.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVAD::JAC) <date>
*    History :
*     date:  changes (institution::username)
*     3-MAY-1984  first insertion (REVAD::JAC)
*     10-OCT-1984  change "normal" to "adam__OK" (REVA::ADAM)
*     14-MAY-1985  turn PATHOPEN status into adam__ok (REVAD::BDK)
*     12.11.1992:  use SAI__OK not ADAM__OK 
*                  rename MESERRS MESSYS_ERR (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_ERR'
*    Import :
      CHARACTER TASK_NAME*(*)   !  name of task to which path is required
*    Export :
      INTEGER PATH              !  pointer to the path
*    Status :
      INTEGER STATUS
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

         CALL MESSYS_PATH ( TASK_NAME, PATH, STATUS )

         IF ( STATUS .EQ. MESSYS__PATHOPEN ) STATUS = SAI__OK

      ENDIF

      END

