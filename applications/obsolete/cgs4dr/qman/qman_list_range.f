*+  QMAN_LIST_RANGE - Lists all records in specified range
      SUBROUTINE QMAN_LIST_RANGE( STATUS )
*    Invocation :
*     CALL QMAN_LIST_RANGE( STATUS )
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
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN_LIST_RANGE common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN_LIST_RANGE global parameter constants
*    Local variables :
      INTEGER START                   ! Start record
      INTEGER END                     ! End record
      INTEGER ITEMP                   ! Temporary variable
      INTEGER ICOUNT                  ! A counter
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Write out informational message
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'Rec# Timestamp             Command', STATUS )
      CALL MSG_OUT( ' ', '---- ---------             -------', STATUS )
      CALL MSG_BLANK( STATUS )

*   Get the range
      CALL PAR_GET0I( 'START', START, STATUS )
      CALL PAR_GET0I( 'END', END, STATUS )

*   Swap them around as necessary
      IF ( START .GT. END ) THEN
        ITEMP = START
        START = END
        END   = ITEMP
      ENDIF

*   List the specified range of records
      DO ICOUNT = START, END, 1
        CALL MSG_FMTI( 'RECNUM', 'I4', ICOUNT )
        CALL MSG_FMTD( 'DATE', 'F21.14', DATEQ( ICOUNT ) )
        CALL MSG_SETC( 'COMMAND', CHARQ( ICOUNT ) )
        CALL MSG_OUT( ' ', '^RECNUM ^DATE ^COMMAND', STATUS )
        CALL MSG_SYNC( STATUS )
      END DO
      CALL MSG_BLANK( STATUS )

*    Exit subroutine
      END
