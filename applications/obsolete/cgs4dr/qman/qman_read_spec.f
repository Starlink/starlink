*+  QMAN_READ_SPEC - Reads a specified record
      SUBROUTINE QMAN_READ_SPEC( STATUS )
*    Invocation :
*     CALL QMAN_READ_SPEC( STATUS )
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

*   Get the record number
      CALL PAR_GET0I( 'NUMBER', READREC_PTR, STATUS )

*   Read the record
      CALL QMAN_READ_REC( STATUS )

*   Exit subroutine
      END
