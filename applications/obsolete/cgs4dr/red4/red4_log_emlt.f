*+  RED4_LOG_EMLT - Write time-stamped EMLT info to the log file
      SUBROUTINE RED4_LOG_EMLT( STATUS )
*    Description :
*     This routine writes time-stamped records to the engineering
*     log file recording the information obtained from the EMLT.LIS file.
*    Invocation :
*     CALL RED4_LOG_EMLT( STATUS )
*    Authors :
*     P N Daly (JACH.HAWAII.EDU::PND)
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
      INCLUDE 'RED4_ENG.INC' ! RED4 (engineering) common block
*    External references :
      INTEGER CHR_LEN        ! Character length determining function
*    Local variables :
      CHARACTER*132 LINE     ! Line to be written to the log file
      CHARACTER*80 STAMP     ! Time stamp
      INTEGER
     :  CPOS                 ! Character position
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that the log file has been opened
      IF ( .NOT. LOG_OPEN ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_LOG_EMLT: Engineering log file not open!', STATUS )
      ENDIF

*    Check there is some valid EMLT information to be recorded.
      IF ( .NOT. EMLT_VALID ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_LOG_EMLT: No valid EMLT information! ', STATUS )
      END IF

*    Get YCEN and Obtain a time stamp
      CALL PAR_GET0R( 'YCEN', YCEN, STATUS )
      CALL RED4_TIME_STAMP( STAMP, STATUS )

*    Put together a string describing the line centre and width to be written to the file
      CALL CHR_FILL( ' ', LINE )
      CPOS = 0
      CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LINE, CPOS )
      CALL CHR_PUTC( 'Line detected at Y = ', LINE, CPOS )
      CALL CHR_PUTR( YCEN, LINE, CPOS )
      CALL CHR_PUTC( ': X centroid = ', LINE, CPOS )
      CALL CHR_PUTR( XCEN, LINE, CPOS )
      CALL CHR_PUTC( ' (', LINE, CPOS )
      CALL CHR_PUTR( BCEN, LINE, CPOS )
      CALL CHR_PUTC( ' bins); FWHM = ', LINE, CPOS )
      CALL CHR_PUTR( XFWHM, LINE, CPOS )
      CALL CHR_PUTC( ' (', LINE, CPOS )
      CALL CHR_PUTR( BFWHM, LINE, CPOS )
      CALL CHR_PUTC( ' bins)', LINE, CPOS )
      CALL  FIO_WRITE( LOG_UNIT, LINE(1:CPOS), STATUS )

*    Put together another string describing the line's strength
      CALL CHR_FILL( ' ', LINE )
      CPOS = 0
      CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LINE, CPOS )
      CALL CHR_PUTC( '      Integrated line strength = ', LINE, CPOS )
      CALL CHR_PUTR( STRENGTH, LINE, CPOS )
      CALL CHR_PUTC( '; Peak height = ', LINE, CPOS )
      CALL CHR_PUTR( PEAK, LINE, CPOS )
      CALL FIO_WRITE( LOG_UNIT, LINE(1:CPOS), STATUS )

*    Put together a string describing the mean line width to be written to the file
      CALL CHR_FILL( ' ', LINE )
      CPOS = 0
      CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LINE, CPOS )
      CALL CHR_PUTC( '      Mean width of ALL the lines = ', LINE, CPOS )
      CALL CHR_PUTR( MEANXFWHM, LINE, CPOS )
      CALL CHR_PUTC( ' X units (', LINE, CPOS )
      CALL CHR_PUTR( MEANBFWHM, LINE, CPOS )
      CALL CHR_PUTC( ' bins)', LINE, CPOS )
      CALL FIO_WRITE( LOG_UNIT, LINE(1:CPOS), STATUS )
      END
