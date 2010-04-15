*+  QMAN_SAVE - SAVE action for QMAN task
      SUBROUTINE QMAN_SAVE( STATUS )
*    Invocation :
*     CALL QMAN_SAVE( STATUS )
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
*    Extermnal references :
       INTEGER CHR_LEN                ! Finds used length of string
*    Local variables :
       CHARACTER*( MSG_VAL_LEN ) FILE ! Output filename
       CHARACTER*( MSG_VAL_LEN ) MODE ! Output mode
       CHARACTER*( MSG_VAL_LEN ) CDAT ! Date string in character format
       CHARACTER*( MSG_VAL_LEN ) TEMP ! Temporary variable
       INTEGER DLEN                   ! Length of string
       INTEGER CLEN                   ! Length of string
       INTEGER FD                     ! File descriptor
       INTEGER ICOUNT                 ! Counter
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check task has been initialised
      CALL QMAN_INIT_DONE( STATUS )

*   Check the password (if specified)
      CALL QMAN_CHECK_PWD( STATUS )

*   Get the output file
      CALL CHR_FILL( ' ', FILE )
      CALL PAR_GET0C( 'FILE', FILE, STATUS )
      CALL CHR_FILL( ' ', MODE )
      CALL PAR_GET0C( 'SAVE_MODE', MODE, STATUS )
      CALL CHR_UCASE( MODE )

*   Open it for write access
      IF ( MODE .EQ. 'NEW' ) THEN
         CALL FIO_OPEN( FILE(1:CHR_LEN(FILE)), 'WRITE',
     :     'LIST', 0, FD, STATUS )
      ELSE
         CALL FIO_OPEN( FILE(1:CHR_LEN(FILE)), 'APPEND',
     :     'LIST', 0, FD, STATUS )
      ENDIF
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'QMAN_SAVE: '/
     :    /'Unable to open output file', STATUS )

*   Write the records
      ELSE
        IF ( MODE .EQ. 'NEW' ) THEN
          DO ICOUNT = 0, MAX_QENTRIES, 1

*         Check first that record is worth preserving
            IF ( ( CHARQ( ICOUNT ) .NE. ' ' ) .AND.
     :           ( DATEQ( ICOUNT ) .NE. 0.0 ) ) THEN
              CALL TASK_ENC0D( DATEQ( ICOUNT ), CDAT, STATUS )
              DLEN = CHR_LEN( CDAT )
              TEMP = CHARQ( ICOUNT )
              CLEN = CHR_LEN( TEMP )
              CALL  FIO_WRITE( FD, CDAT(1:DLEN)//' '/
     :          /TEMP(1:CLEN), STATUS )
            ENDIF
          ENDDO
        ELSE

*       Append the current record only
          CALL TASK_ENC0D( DATEQ( MAXREC_PTR ), CDAT, STATUS )
          DLEN = CHR_LEN( CDAT )
          TEMP = CHARQ( MAXREC_PTR )
          CLEN = CHR_LEN( TEMP )
          CALL  FIO_WRITE( FD, CDAT(1:DLEN)//' '/
     :      /TEMP(1:CLEN), STATUS )
        ENDIF

*     Close the file
        CALL FIO_CLOSE( FD, STATUS )
      ENDIF

*   Exit subroutine
      END
