*+  QMAN_LIST - List action for QMAN task
      SUBROUTINE QMAN_LIST( STATUS )
*    Invocation :
*     CALL QMAN_LIST( STATUS )
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
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN_LIST common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN_LIST global parameter constants
*    Local variables :
      CHARACTER*10 MODE               ! The acccess mode
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check task has been initialised
      CALL QMAN_INIT_DONE( STATUS )

*   Check the password (if specified)
      CALL QMAN_CHECK_PWD( STATUS )

*   Get the mode
      CALL CHR_FILL( ' ', MODE )
      CALL PAR_GET0C( 'LIST_MODE', MODE, STATUS )
      CALL CHR_UCASE( MODE )

*   If no records are in use, say so
      IF ( USED_RECORDS .LE. 0 ) THEN
        CALL MSG_OUT( ' ', 'The queue is empty', STATUS )

*   List all records
      ELSE IF ( MODE .EQ. 'ALL' ) THEN
        CALL QMAN_LIST_ALL( STATUS )

*   List a range of records
      ELSE IF ( MODE .EQ. 'RANGE' )THEN
        CALL QMAN_LIST_RANGE( STATUS )

*   List a specified records
      ELSE IF ( MODE .EQ. 'SPECIFIED' )THEN
        CALL QMAN_LIST_SPEC( STATUS )
      ENDIF

*   Write out a verbose mode message
      IF ( VERBOSE ) THEN
        CALL MSG_SETI( 'UR', USED_RECORDS )
        CALL MSG_OUT( ' ', 'Number of used records = ^UR', STATUS )
        CALL MSG_SETI( 'MI', MINREC_PTR )
        CALL MSG_OUT( ' ', 'Minimum record pointer = ^MI', STATUS )
        CALL MSG_SETI( 'MA', MAXREC_PTR )
        CALL MSG_OUT( ' ', 'Maximum record pointer = ^MA', STATUS )
      ENDIF

*   Exit subroutine
      END
