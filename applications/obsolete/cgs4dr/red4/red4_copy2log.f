*+  RED4_COPY2LOG - Copy the contents of a text file to the log file
      SUBROUTINE RED4_COPY2LOG( STATUS )
*    Description :
*     This routine copies each of the lines found in a specified text
*     file to the engineering log file, preceding each line with a
*     time stamp.
*    Invocation :
*     CALL RED4_COPY2LOG( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     19-Jan-1990: Original Unix version                             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
*    Status :
      INTEGER STATUS               ! Global status
*    External references :
      INTEGER CHR_LEN              ! Character length determining function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'    ! RED4 common block
      INCLUDE 'RED4_ENG.INC'       ! RED4 (engineering) common block
*    Local variables :
      CHARACTER*132
     :  SRC_LINE,                  ! Line read from SRC file
     :  LOG_LINE                   ! Line to be written to the log file
      CHARACTER*80
     :  SRC_FILE,                  ! Name of SRC file
     :  STAMP                      ! Time stamp
      CHARACTER*30
     :  DELIMITER                  ! Delimiter used to mark the start and end of file.
      INTEGER
     :  SRC_UNIT,                  ! Logical unit number for SRC file
     :  CPOS,                      ! Position in character string
     :  CLEN                       ! Length of character string
*    Local data :
      DATA DELIMITER / '------------------------------' /
*-

*    Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that the log file has been opened
      IF ( .NOT. LOG_OPEN ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_COPY2LOG: Logfile has not been opened!', STATUS )
      ENDIF

*    Obtain the name of the SRC file to be copied and open it
      CALL PAR_GET0C( 'SOURCE_FILE', SRC_FILE, STATUS )
      CLEN = INDEX( SRC_FILE, SEPARATOR )
      SRC_FILE = CGS4_ENG(1:CHR_LEN(CGS4_ENG)) // SRC_FILE(CLEN+1:CHR_LEN(SRC_FILE))
      CALL FIO_OPEN( SRC_FILE(1:CHR_LEN(SRC_FILE)), 'READ', 'LIST', 132, SRC_UNIT, STATUS )

*    Obtain a time stamp
      CALL RED4_TIME_STAMP( STAMP, STATUS )

*    Write a message to the log file
      CALL CHR_FILL( ' ', LOG_LINE )
      CPOS = 0
      CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LOG_LINE, CPOS )
      CALL CHR_PUTC( DELIMITER, LOG_LINE, CPOS )
      CALL CHR_PUTC( ' Contents of ', LOG_LINE, CPOS )
      CALL CHR_PUTC( SRC_FILE(1:CHR_LEN(SRC_FILE))//' ', LOG_LINE, CPOS )
      CALL CHR_PUTC( DELIMITER, LOG_LINE, CPOS )
      CALL FIO_WRITE( LOG_UNIT, LOG_LINE(1:CPOS), STATUS )

*    Loop and read the lines from the SRC file
      DO WHILE ( STATUS.EQ.SAI__OK )
         CALL FIO_READ( SRC_UNIT, SRC_LINE, CLEN, STATUS )

*       Prefix the SRC line with the time stamp
         CALL CHR_FILL( ' ', LOG_LINE )
         CPOS = 0
         CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LOG_LINE, CPOS )
         CALL CHR_PUTC( SRC_LINE(1:CLEN), LOG_LINE, CPOS )
         CALL FIO_WRITE( LOG_UNIT, LOG_LINE(1:CPOS), STATUS )
      ENDDO

*    Reset status if we hit end of file
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*    Write a message to the log file delimiting the end of the SRC file
      CPOS = 0
      CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LOG_LINE, CPOS )
      CALL CHR_PUTC( DELIMITER, LOG_LINE, CPOS )
      CALL CHR_PUTC( ' End of ', LOG_LINE, CPOS )
      CALL CHR_PUTC( SRC_FILE(1:CHR_LEN(SRC_FILE))//' ', LOG_LINE, CPOS )
      CALL CHR_PUTC( DELIMITER, LOG_LINE, CPOS )
      CALL FIO_WRITE( LOG_UNIT, LOG_LINE(1:CPOS), STATUS )

*    Close the SRC file.
      CALL FIO_CLOSE( SRC_UNIT, STATUS )
      END
