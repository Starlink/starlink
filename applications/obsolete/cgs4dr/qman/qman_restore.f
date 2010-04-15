*+  QMAN_RESTORE - RESTORE action for QMAN task
      SUBROUTINE QMAN_RESTORE( STATUS )
*    Invocation :
*     CALL QMAN_RESTORE( STATUS )
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
*    External references :
      INTEGER CHR_LEN                 ! Finds used length of string
*    Local variables :
      CHARACTER*( MSG_VAL_LEN ) FILE  ! Output filename
      CHARACTER*( MSG_VAL_LEN+36 )
     :   CREC                         ! Input from file
      CHARACTER*( MSG_VAL_LEN+36 )
     :   DREC                         ! Input from file
      INTEGER DLEN                    ! Length of string
      INTEGER CLEN                    ! Length of string
      INTEGER FD                      ! File descriptor
      DOUBLE PRECISION DATE_TMP       ! Temporary variable
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

*   Open it for read access
      CALL FIO_OPEN( FILE(1:CHR_LEN(FILE)), 'READ',
     :  'LIST', 0, FD, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'QMAN_RESTORE: '/
     :    /'Unable to open input file', STATUS )

*   Read the records and decode
      ELSE
        IF ( VERBOSE ) THEN
          CALL MSG_SETC( 'FILE', FILE )
          CALL MSG_OUT( ' ', 'Reading from file ^FILE', STATUS )
        ENDIF

*     Loop and read records
        DO WHILE ( STATUS .EQ. SAI__OK )
          CALL QMAN_CHECK_SLOT( STATUS )
          CREC = ' '
          DREC = ' '
          CALL FIO_READ( FD, CREC, CLEN, STATUS )
          DREC = CREC(1:CLEN)

*       Decode date
          DATE_TMP = 0.0
          DLEN = 0
          CALL CHR_TRUNC( ' ', DREC )
          DLEN = CHR_LEN( DREC )
          CALL TASK_DEC0D( DREC(1:DLEN), DATE_TMP, STATUS )
          IF ( VERBOSE ) THEN
            CALL MSG_FMTD( 'DATE', '(F21.12)', DATE_TMP )
            CALL MSG_OUT( ' ', 'Decoded date into ^DATE', STATUS )
          ENDIF

*       Decode rest of string
          DREC = CREC(DLEN+2:LEN(CREC))
          DLEN = CHR_LEN( DREC )
          IF ( VERBOSE ) THEN
            CALL MSG_SETC( 'REC', DREC(1:DLEN) )
            CALL MSG_OUT( ' ', 'Command string is ^REC', STATUS )
          ENDIF

          IF ( ( DREC(1:DLEN) .NE. ' ' ) .AND.
     :         ( DATE_TMP .NE. 0.0 )     .AND.
     :         ( STATUS .EQ. SAI__OK ) ) THEN
            MAXREC_PTR = MAXREC_PTR + 1
            DATEQ( MAXREC_PTR ) = DATE_TMP
            CHARQ( MAXREC_PTR ) = DREC(1:DLEN)
            USED_RECORDS = MAXREC_PTR - MINREC_PTR + 1
          ENDIF
        ENDDO

*     Reset the status as it should have been set to FIO__EOF
        CALL ERR_ANNUL( STATUS )

*     Close the file
        CALL FIO_CLOSE( FD, STATUS )
      ENDIF

*   Exit subroutine
      END
