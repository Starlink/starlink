*+  QMAN_WRITE - Write action for QMAN task
      SUBROUTINE QMAN_WRITE( STATUS )
*    Invocation :
*     CALL QMAN_WRITE( STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*    History :
*     27-May-1994: Original version                                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'                ! Defines SAI__OK and others
      INCLUDE 'MESSYS_LEN'                  ! Defines MSG_VAL_LEN etc
*    Status :
      INTEGER STATUS                   ! Inherited global ADAM status
*    Global variables :
      INCLUDE 'QMAN_GLOBAL.PAR'        ! QMAN common block
      INCLUDE 'QMAN_COMMON.BLK'        ! QMAN global parameter constants
*    External references :
      INTEGER CHR_LEN                  ! Finds used length of string
*    Local variables :
      CHARACTER*6 QPOSITION            ! Position in queue
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check task has been initialised
      CALL QMAN_INIT_DONE( STATUS )

*   Check the password (if specified)
      CALL QMAN_CHECK_PWD( STATUS )

*   Check that a slot is available
      CALL QMAN_CHECK_SLOT( STATUS )

*   Get a date-time stamp
      CALL QMAN_TIMESTAMP( STATUS )

*   Get the queue position and negate timestamp as appropriate
      CALL CHR_FILL( ' ', QPOSITION, STATUS )
      CALL PAR_GET0C( 'QPOSITION', QPOSITION, STATUS )
      CALL CHR_UCASE( QPOSITION )
      IF ( QPOSITION .NE. 'NEWEST' ) TIMESTAMP = -TIMESTAMP

*   Get the command string for the database
      CALL CHR_FILL( ' ', STRING )
      CALL PAR_GET0C( 'STRING', STRING, STATUS )

*   Write the record to the database
      IF ( STATUS .EQ. SAI__OK ) THEN

        MAXREC_PTR = MAXREC_PTR + 1
        CHARQ( MAXREC_PTR ) = STRING(1:CHR_LEN(STRING))
        DATEQ( MAXREC_PTR ) = TIMESTAMP
        USED_RECORDS = MAXREC_PTR - MINREC_PTR + 1
        IF ( VERBOSE ) THEN
          CALL MSG_SETI( 'RP', MAXREC_PTR )
          CALL MSG_FMTD( 'DT', '(F21.12)', DATEQ( MAXREC_PTR ) )
          CALL MSG_SETC( 'RC', CHARQ( MAXREC_PTR ) )
          CALL MSG_OUT( ' ', 'Writing ^DT ^RC at ^RP', STATUS )
        ENDIF
      ELSE

        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'QMAN_WRITE: '/
     :    /'Unable to write database record', STATUS )
      ENDIF

*   Always make sure record 0 contains no values
      CALL CHR_FILL( ' ', CHARQ(0) )
      DATEQ(0) = 0.0

*   Exit subroutine
      END
