*+  QMAN_LIST_ALL - Lists all queue entries currently in use
      SUBROUTINE QMAN_LIST_ALL( STATUS )
*    Invocation :
*     CALL QMAN_LIST_ALL( STATUS )
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
      INTEGER ICOUNT                  ! A counter
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Write out a title block
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'Rec# Timestamp             Command', STATUS )
      CALL MSG_OUT( ' ', '---- ---------             -------', STATUS )
      CALL MSG_BLANK( STATUS )

*   List all records
      DO ICOUNT = 0, MAX_QENTRIES, 1
        IF ( CHARQ(ICOUNT).NE.' ' .AND. DATEQ(ICOUNT).NE.0.0 ) THEN
          CALL MSG_FMTI( 'RECNUM', 'I4', ICOUNT )
          CALL MSG_FMTD( 'DATE', 'F21.14', DATEQ( ICOUNT ) )
          CALL MSG_SETC( 'COMMAND', CHARQ( ICOUNT ) )
          CALL MSG_OUT( ' ', '^RECNUM ^DATE ^COMMAND', STATUS )
          CALL MSG_SYNC( STATUS )
        END IF
      END DO
      CALL MSG_BLANK( STATUS )

*    Exit subroutine
      END
