*+  QMAN_DELETE_SPEC - Deletes a specified record
      SUBROUTINE QMAN_DELETE_SPEC( STATUS )
*    Invocation :
*     CALL QMAN_DELETE_SPEC( STATUS )
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
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN_DELETE common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN_DELETE global parameter constants
*    Local variables :
      INTEGER NUMBER                  ! A record number
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the record number
      CALL PAR_GET0I( 'NUMBER', NUMBER, STATUS )
      CALL MSG_SETI( 'RN', NUMBER )
      CALL MSG_OUT( ' ', 'Deleting record number ^RN', STATUS )

*   Delete the specified record
      CHARQ( NUMBER ) = ' '
      DATEQ( NUMBER ) = 0.0

*   Re-calculate the pointers
      IF ( USED_RECORDS .GT. 0 ) THEN
        IF ( NUMBER .EQ. MINREC_PTR ) MINREC_PTR = MINREC_PTR + 1
        IF ( NUMBER .EQ. MAXREC_PTR ) MAXREC_PTR = MAXREC_PTR - 1
        USED_RECORDS = MAXREC_PTR - MINREC_PTR + 1
      ENDIF

*   Exit subroutine
      END
