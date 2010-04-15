*+  QMAN_DELETE_ALL - Deletes all queue entries
      SUBROUTINE QMAN_DELETE_ALL( STATUS )
*    Invocation :
*     CALL QMAN_DELETE_ALL( STATUS )
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
*    Local variables :
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Delete all records
      CALL MSG_OUT( ' ', 'Deleting all queue entries', STATUS )
      CALL QMAN_INIT_POINTERS( STATUS )

*    Exit subroutine
      END
