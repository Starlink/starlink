*+  RED4_LOG_COMMENT - Write a time-stamped comment to the log file
      SUBROUTINE RED4_LOG_COMMENT( STATUS )
*    Description :
*     This routine writes a time-stamped comment to the engineering log file.
*    Invocation :
*     CALL RED4_LOG_COMMENT( STATUS )
*    Authors :
*     P N Daly  (JACH.HAWAII.EDU::PND)
*    History :
*     19-Jan-1995: Original Unix version.                            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS         ! Global status
*    Global variables :
      INCLUDE 'RED4_ENG.INC'
*    External references :
      INTEGER CHR_LEN        ! Character length determining function
*    Local variables :
      CHARACTER*132
     :  COMMENT,     ! Comment to be written to the log file
     :  LINE         ! Line to be written to the log file
      CHARACTER*80
     :  STAMP        ! Time stamp
      INTEGER
     :  CPOS         ! Character position
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that the log file has been opened
      IF ( .NOT. LOG_OPEN ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_LOG_COMMENT: Engineering logfile not open!', STATUS )
      ENDIF

*    Obtain the comment to be written to the log file
      CALL PAR_GET0C( 'COMMENT', COMMENT, STATUS )

*    Obtain a time stamp
      CALL RED4_TIME_STAMP( STAMP, STATUS )

*    Combine the time stamp and the comment into a string
      CALL CHR_FILL( ' ', LINE )
      CPOS = 0
      CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LINE, CPOS )
      CALL CHR_PUTC( COMMENT(1:CHR_LEN(COMMENT)), LINE, CPOS )

*    Write the string to the log file.
      CALL FIO_WRITE( LOG_UNIT, LINE(1:CPOS), STATUS )
      END
