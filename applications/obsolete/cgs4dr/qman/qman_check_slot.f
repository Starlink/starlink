*+  QMAN_CHECK_SLOT - Checks that a slot is available for command
      SUBROUTINE QMAN_CHECK_SLOT( STATUS )
*    Invocation :
*     CALL QMAN_CHECK_SLOT( STATUS )
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
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN_CHECK_SLOT common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN_CHECK_SLOT global parameter constants
*    Local variables :
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   If the current record pointer is too big try to recover gaps
      IF ( USED_RECORDS .GE. MAX_QENTRIES ) CALL QMAN_REORDER( STATUS )

*   If it's still too big, signal an error
      IF ( ( USED_RECORDS .GE. MAX_QENTRIES ) .OR.
     :     ( STATUS .NE. SAI__OK ) ) THEN

        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'QMAN_CHECK_SLOT: '/
     :    /'No slot available for record', STATUS )
      ENDIF

*    Exit subroutine
      END
