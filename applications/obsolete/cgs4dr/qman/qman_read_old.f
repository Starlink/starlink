*+  QMAN_READ_OLD - Reads oldest record
      SUBROUTINE QMAN_READ_OLD( STATUS )
*    Invocation :
*     CALL QMAN_READ_OLD( STATUS )
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

*   Get the record at the minimum time
      CALL QMAN_MINTIME( STATUS )

*   Read the record
      CALL QMAN_READ_REC( STATUS )

*   Exit subroutine
      END
