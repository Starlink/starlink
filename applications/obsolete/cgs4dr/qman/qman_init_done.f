*+  QMAN_INIT_DONE - Checks if the task has been initialised
      SUBROUTINE QMAN_INIT_DONE( STATUS )
*    Invocation :
*     CALL QMAN_INIT_DONE( STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*    History :
*     27-May-1994: Original version                                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'               ! Defines SAI__OK and others
      INCLUDE 'MESSYS_LEN'                 ! Defines MSG_VAL_LEN etc
*    Status :
      INTEGER STATUS                  ! Inherited global ADAM status
*    Global variables :
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN_INIT_DONE common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN_INIT_DONE global parameter constants
*    Local variables :
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Has the task been initialised?
      IF ( .NOT. INITIALISED ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ',
     :    'QMAN_INIT_DONE: Task has not been initialised', STATUS )
      ENDIF

*    Exit subroutine
      END
