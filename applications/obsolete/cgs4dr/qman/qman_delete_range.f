*+  QMAN_DELETE_RANGE - Deletes a range of records and re-orders queue
      SUBROUTINE QMAN_DELETE_RANGE( STATUS )
*    Invocation :
*     CALL QMAN_DELETE_RANGE( STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*    History :
*     27-May-1994: Original version                                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'               ! Defines SAI__OK and others
      INCLUDE 'MESSYS_LEN'                 ! Defines MSSG_VAL_LEN etc
*    Status :
      INTEGER STATUS                  ! Inherited global ADAM status
*    Global variables :
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN global parameter constants
*    Local variables :
      INTEGER START                   ! A record number
      INTEGER END                     ! A record number
      INTEGER ITEMP                   ! A temporary variable
      INTEGER ICOUNT                  ! A counter
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the start and end values
      CALL PAR_GET0I( 'START', START, STATUS )
      CALL PAR_GET0I( 'END', END, STATUS )

*   Reverse the start and end if specified the wrong way aroung
      IF ( START .GT. END ) THEN
        ITEMP = END
        END   = START
        START = ITEMP
      END IF

*   Put out an informational message
      IF ( VERBOSE ) THEN
        CALL MSG_SETI( 'RS', START )
        CALL MSG_SETI( 'RE', END )
        CALL MSG_OUT( ' ', 'Deleting records ^RS to ^RE', STATUS )
      END IF

*   Now delete the records
      DO ICOUNT = START, END, 1
        CHARQ( ICOUNT ) = ' '
        DATEQ( ICOUNT ) = 0.0
      END DO

*   Re-calculate range of records
      CALL QMAN_REORDER( STATUS )

*   Exit subroutine
      END
