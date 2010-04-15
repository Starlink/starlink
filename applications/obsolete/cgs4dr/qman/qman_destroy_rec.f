*+  QMAN_DESTROY_REC - Destroys a record
      SUBROUTINE QMAN_DESTROY_REC( STATUS )
*    Invocation :
*     CALL QMAN_DESTROY_REC( STATUS )
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
      LOGICAL DESTRUCTIVE             ! T if we are to delete record after read
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Proceed only if we read a record OK and pointer is non-zero
      IF ( ( READREC_OK ) .AND. ( READREC_PTR .GT. 0 ) ) THEN

*     If read in destructive mode, delete the record
        DESTRUCTIVE = .FALSE.
        CALL PAR_GET0L( 'DESTRUCTIVE', DESTRUCTIVE, STATUS )
        IF ( DESTRUCTIVE ) THEN
          IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :      'Destroying record', STATUS )
          CHARQ( READREC_PTR ) = ' '
          DATEQ( READREC_PTR ) = 0.0
          IF ( READREC_PTR .EQ. MINREC_PTR ) MINREC_PTR = MINREC_PTR + 1
          IF ( READREC_PTR .EQ. MAXREC_PTR ) MAXREC_PTR = MAXREC_PTR - 1
          USED_RECORDS = MAXREC_PTR - MINREC_PTR + 1
        ENDIF
      ENDIF

*   Exit subroutine
      END
