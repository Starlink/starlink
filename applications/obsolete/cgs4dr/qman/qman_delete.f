*+  QMAN_DELETE - Delete action for QMAN task
      SUBROUTINE QMAN_DELETE( STATUS )
*    Invocation :
*     CALL QMAN_DELETE( STATUS )
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
      CHARACTER*10 MODE               ! Access mode for record
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check task has been initialised
      CALL QMAN_INIT_DONE( STATUS )

*   Check the password (if specified)
      CALL QMAN_CHECK_PWD( STATUS )

*   Get record access mode
      CALL CHR_FILL( ' ', MODE )
      CALL PAR_GET0C( 'DELETE_MODE', MODE, STATUS )
      CALL CHR_UCASE( MODE )

*   If no records in use, say so
      IF ( USED_RECORDS .LE. 0 ) THEN
        CALL MSG_OUT( ' ', 'The queue is already empty', STATUS )

*   Delete all record and reset pointers
      ELSE IF ( MODE .EQ. 'ALL' ) THEN
         CALL QMAN_DELETE_ALL( STATUS )

*   Delete a range of records and re-order
      ELSE IF ( MODE .EQ. 'RANGE' ) THEN
         CALL QMAN_DELETE_RANGE( STATUS )

*   Delete a specified record and re-order
      ELSE IF ( MODE .EQ. 'SPECIFIED' ) THEN
         CALL QMAN_DELETE_SPEC( STATUS )
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
