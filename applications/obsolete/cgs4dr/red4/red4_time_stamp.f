*+  RED4_TIME_STAMP - Obtain a time stamp string from the system
      SUBROUTINE RED4_TIME_STAMP( STAMP, STATUS )
*    Description :
*     This routine obtains a time stamp from the system.
*    Invocation :
*     CALL RED4_TIME_STAMP( STAMP, STATUS )
*    Authors :
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     19-Jan-1990: Original Unix version           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Export :
      CHARACTER*(*) STAMP        ! A string to contain the current date and time
*    Status :
      INTEGER STATUS             ! Global status
*    Local variables :
      INTEGER NTICKS, SECS, MINS, HOURS
      INTEGER DAY, MONTH, YEAR
      INTEGER WDAY, YDAY, ISDST, TSTRCT, CPOS
      CHARACTER*3 CMONTH(0:11)
*    Local data :
      DATA CMONTH / 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain the current date and time
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_LOCALTIME( NTICKS, SECS, MINS, HOURS, DAY, MONTH, YEAR, WDAY, YDAY, ISDST, TSTRCT, STATUS )

*   Form an output string
      CALL CHR_FILL( ' ', STAMP )
      CPOS = 0
      IF ( DAY .LE. 9 ) CALL CHR_PUTC( '0', STAMP, CPOS )
      CALL CHR_PUTI( DAY, STAMP, CPOS )
      CALL CHR_PUTC( '-', STAMP, CPOS )
      CALL CHR_PUTC( CMONTH(MONTH), STAMP, CPOS )
      CALL CHR_PUTC( '-', STAMP, CPOS )
      CALL CHR_PUTI( YEAR, STAMP, CPOS )
      CALL CHR_PUTC( ':', STAMP, CPOS )
      IF ( HOURS .LE. 9 ) CALL CHR_PUTC( '0', STAMP, CPOS )
      CALL CHR_PUTI( HOURS, STAMP, CPOS )
      CALL CHR_PUTC( ':', STAMP, CPOS )
      IF ( MINS .LE. 9 ) CALL CHR_PUTC( '0', STAMP, CPOS )
      CALL CHR_PUTI( MINS, STAMP, CPOS )
      CALL CHR_PUTC( ':', STAMP, CPOS )
      IF ( SECS .LE. 9 ) CALL CHR_PUTC( '0', STAMP, CPOS )
      CALL CHR_PUTI( SECS, STAMP, CPOS )
      CALL CHR_RMBLK( STAMP )

      END
