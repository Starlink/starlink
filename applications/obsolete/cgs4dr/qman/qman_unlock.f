*+  QMAN_UNLOCK - UNLOCK routine for QMAN task
      SUBROUTINE QMAN_UNLOCK( STATUS )
*    Description :
*    Invocation :
*     CALL QMAN_UNLOCK( STATUS )
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

*   Check the password (if specified) - this also checks the lockword
      CALL QMAN_CHECK_PWD( STATUS )

*   Annul the lockword parameter
      CALL PAR_PUT0C( 'LOCKWORD', ' ', STATUS )

*   Set the DB_UNLOCKED flag
      DB_LOCKED = .FALSE.
      IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :      'Database unlocked', STATUS )

*    Exit subroutine
      END
