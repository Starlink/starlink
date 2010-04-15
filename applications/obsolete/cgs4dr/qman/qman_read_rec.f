*+  QMAN_READ_REC - Reads a record
      SUBROUTINE QMAN_READ_REC( STATUS )
*    Invocation :
*     CALL QMAN_READ_REC( STATUS )
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
*    External references :
      INTEGER CHR_LEN                 ! Function to find used length of string
*    Local variables :
      CHARACTER*( MSG_VAL_LEN ) CTEMP ! Temporary record
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Read the record
      CTEMP = CHARQ( READREC_PTR )
      CALL PAR_PUT0C( 'OUTPUT', CTEMP(1:CHR_LEN(CTEMP)), STATUS )
      CALL TASK_PUT_VALUE( CTEMP(1:CHR_LEN(CTEMP)), STATUS )
      READREC_OK = .TRUE.

*   Exit subroutine
      END
