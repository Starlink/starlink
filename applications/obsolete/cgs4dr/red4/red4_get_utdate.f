*+  RED4_GET_UTDATE - Returns UT date of form YYYYMMDD or part thereof in string
      SUBROUTINE RED4_GET_UTDATE( UTDATE, STATUS )
*    Description :
*     Get the UTdate in a specified form
*    Authors :
*     Phil Daly (JACH::PND)
*    History :
*      6-Sep-1994: Original Unix version (PND)
*     29-Nov-1994: Use PSX calls to get local date (AB)
*    endhistory
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'            ! Status definitions
*    External functions :
      INTEGER CHR_LEN              ! Finds length of string
*    Status :
      INTEGER STATUS               ! Inherited status value
*    Import-Export :
      CHARACTER*(*) UTDATE         ! String to write UTdate into
*    Local variables :
      CHARACTER*8 CDATE            ! Temporary date string
      INTEGER CPOS                 ! String position
      INTEGER NTICKS               ! For PSX call
      INTEGER HOUR                 ! hour in day
      INTEGER DAY                  ! Day number
      INTEGER MONTH                ! Month number
      INTEGER YEAR                 ! Year number
      INTEGER DUMMY                ! Dummy integer
      DOUBLE PRECISION MJD         ! Modified julian date
      DOUBLE PRECISION FD          ! Fraction of MJD (not used)
*-

*    Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise variables
      CALL CHR_FILL( ' ', CDATE )
      CALL CHR_FILL( ' ', UTDATE )
      DAY   = 0
      MONTH = 0
      YEAR  = 0
      MJD   = 0.0
      FD    = 0.0

*    Get the present date
      CALL PSX_TIME (NTICKS, STATUS)
      CALL PSX_LOCALTIME (NTICKS, DUMMY, DUMMY, HOUR, DAY, MONTH,
     : YEAR, DUMMY, DUMMY, DUMMY, DUMMY, STATUS)
      MONTH = MONTH + 1

*    Convert to MJD (with century defaults)
      CALL SLA_CALDJ( YEAR, MONTH, DAY, MJD, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_GET_UTDATE: '/
     :    /'Unable to convert date to MJD', STATUS )
      ENDIF

*    if past 14:00 then add a day and re-convert to get present UT
      IF (HOUR .GE. 14) MJD = MJD + 1.0
      CALL SLA_DJCL( MJD, YEAR, MONTH, DAY, FD, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_GET_UTDATE: '/
     :    /'Unable to convert MJD UT-date', STATUS )
      ENDIF

*    Now format the string
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Create a temporary string
        CPOS = 0
        CALL CHR_PUTI( YEAR, CDATE, CPOS )
        IF ( MONTH .LE. 9 ) CALL CHR_PUTC( '0', CDATE, CPOS )
        CALL CHR_PUTI( MONTH, CDATE, CPOS )
        IF ( DAY .LE. 9 ) CALL CHR_PUTC( '0', CDATE, CPOS )
        CALL CHR_PUTI( DAY, CDATE, CPOS )

*      Copy as much out as we can
        CPOS = LEN( CDATE ) - LEN( UTDATE ) + 1
        IF ( CPOS .LE. 0 ) CPOS = 1
        UTDATE = CDATE( CPOS : CHR_LEN(CDATE) )

      ELSE
        CALL CHR_FILL( ' ', UTDATE )
      ENDIF

      END
