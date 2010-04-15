*+  QMAN_LOCK - Lock routine for QMAN task
      SUBROUTINE QMAN_LOCK( STATUS )
*    Description :
*    Invocation :
*     CALL QMAN_LOCK( STATUS )
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
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN global parameter constants
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check task has been initialised
      CALL QMAN_INIT_DONE( STATUS )

*   Check the password (if specified)
      CALL QMAN_CHECK_PWD( STATUS )

*   Get the lockword
      CALL CHR_FILL( ' ', LOCKWORD )
      CALL PAR_GET0C( 'LOCKWORD', LOCKWORD, STATUS )
      CALL PAR_CANCL( 'LOCKWORD', STATUS )

*   Set the DB_LOCKED flag
      DB_LOCKED = .TRUE.
      IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :      'Database locked', STATUS )

*    Exit subroutine
      END
