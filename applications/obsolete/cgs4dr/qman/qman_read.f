*+  QMAN_READ - Read action for QMAN task
      SUBROUTINE QMAN_READ( STATUS )
*    Invocation :
*     CALL QMAN_READ( STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*    History :
*     27-May-1994: Original version                                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'                ! Defines SAI__OK etc
      INCLUDE 'MESSYS_LEN'                  ! Defines MSG_VAL_LEN etc
*    Status :
      INTEGER STATUS                   ! Inherited global ADAM status
*    Global variables :
      INCLUDE 'QMAN_GLOBAL.PAR'        ! QMAN common block
      INCLUDE 'QMAN_COMMON.BLK'        ! QMAN global parameter constants
*    Local variables :
      CHARACTER*10 MODE                ! Read mode
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check task has been initialised
      CALL QMAN_INIT_DONE( STATUS )

*   Check the password (if specified)
      CALL QMAN_CHECK_PWD( STATUS )

*   Get the mode
      CALL CHR_FILL( ' ', MODE )
      CALL PAR_GET0C( 'READ_MODE', MODE, STATUS )
      CALL CHR_UCASE( MODE )

*   Initialise some variables, output record
      CALL PAR_PUT0C( 'OUTPUT', ' ', STATUS )
      READREC_PTR = 0
      READREC_OK  = .FALSE.

*   If no records are in use, say so
      IF ( USED_RECORDS .LE. 0 ) THEN
        CALL PAR_PUT0C( 'OUTPUT', 'No records available', STATUS )
        CALL TASK_PUT_VALUE( 'No records available', STATUS )

*   Read the newest record
      ELSE IF ( MODE .EQ. 'NEWEST' ) THEN
        CALL QMAN_READ_NEW( STATUS )

*   Read the oldest record
      ELSE IF ( MODE .EQ. 'OLDEST' )THEN
        CALL QMAN_READ_OLD( STATUS )

*   Read a record matching a given string
      ELSE IF ( MODE .EQ. 'SEARCH' )THEN
        CALL QMAN_READ_SEARCH( STATUS )

*   Read a specified record
      ELSE IF ( MODE .EQ. 'SPECIFIED' )THEN
        CALL QMAN_READ_SPEC( STATUS )
      ENDIF

*   Destroy the record if necessary
      CALL QMAN_DESTROY_REC( STATUS )

*   Exit subroutine
      END
