*+  RED4_LOG_TYPE1 - Write type 1 engineering record to log file
      SUBROUTINE RED4_LOG_TYPE1( STATUS )
*    Description :
*     This routine writes a time-stamped engineering record of "type 1"
*     to the engineering log file. The record is flagged as a "type 1"
*     by prefixing it with "1:" after the time stamp.
*
*     The record will consist of a set of values in a predefined order.
*     All the records of "type 1" may be extracted from the engineering
*     log file to produce a file with consistent columns of numbers
*     which may be read into SCAR or MONGO. (Type 1 records will be used
*     for testing the detector translation, detector focus, slit
*     rotation and slit positional accuracy, as described in CGS4/GEN/006.1/.
*     It is envisaged that other types of record may appear when other
*     engineering tests are devised).
*
*     A "type 1" record is defined as having the following columns :-
*
*     Obs.    Start  Grating  Set slit  Meas.slit  Focus    Line    Peak  Line
*     number  time   angle    angle     angle      setting  centre  flux  FWHM
*
*    Invocation :
*     CALL RED4_LOG_TYPE1( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     19-Jan-1995: Original Unix version.                              (PND)
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
*    Local constants :
      INTEGER NUMBER_COLUMNS ! The number of columns to be written
      PARAMETER ( NUMBER_COLUMNS = 9 )
      INTEGER COLUMN_WIDTH   ! Width of each output column
      PARAMETER ( COLUMN_WIDTH = 11 )
*    Local variables :
      CHARACTER*132 LINE     ! Line to be written to the log file
      CHARACTER*(COLUMN_WIDTH)
     :  TITLE_TOP( NUMBER_COLUMNS ), ! Top title line for each column
     :  TITLE_BOT( NUMBER_COLUMNS ), ! Bottom title line for each column
     :  COLUMN                       ! Character buffer to contain column
      CHARACTER*80 STAMP     ! Time stamp
      INTEGER
     :  COL,                 ! Column loop counter
     :  CLEN,                ! Length of character string
     :  CPOS                 ! Character position
*    Local data :-
      DATA TITLE_TOP / 'Obs        ', 'Start      ', 'Grating    ',
     :                 'Set slit   ', 'Meas.slit  ', 'Focus      ',
     :                 'Line       ', 'Peak       ', 'Line       ' /
      DATA TITLE_BOT / 'number     ', 'time       ', 'angle      ',
     :                 'angle      ', 'angle      ', 'setting    ',
     :                 'centre     ', 'flux       ', 'FWHM       ' /
*-

*    Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that the log file has been opened
      IF ( .NOT. LOG_OPEN ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_LOG_TYPE1: Engineering log file not open!', STATUS )
      ENDIF

*    Check there is a valid slit angle available
      IF ( .NOT. SLIT_ANGLE_VALID ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_LOG_TYPE1: There is no valid slit angle!', STATUS )
      ENDIF

*    Check that valid parameters have been obtained from an observation file
      IF ( .NOT. OBSERVATION_VALID ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_LOG_TYPE1: There is no valid observation!', STATUS )
      ENDIF

*    Obtain a time stamp
      CALL RED4_TIME_STAMP( STAMP, STATUS )

*    Build up the first title line
      CALL CHR_FILL( ' ', LINE )
      CPOS = 0
      CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LINE, CPOS )
      CALL CHR_PUTC( TITLE_TOP(1), LINE, CPOS )
      DO COL = 2, NUMBER_COLUMNS
         CALL CHR_PUTC( ' ', LINE, CPOS )
         CALL CHR_PUTC( TITLE_TOP(COL), LINE, CPOS )
      ENDDO
      CALL FIO_WRITE( LOG_UNIT, LINE(1:CPOS), STATUS )

*    Build up the second title line
      CALL CHR_FILL( ' ', LINE )
      CPOS = 0
      CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LINE, CPOS )
      CALL CHR_PUTC( TITLE_BOT(1), LINE, CPOS )
      DO COL = 2, NUMBER_COLUMNS
         CALL CHR_PUTC( ' ', LINE, CPOS )
         CALL CHR_PUTC( TITLE_BOT(COL), LINE, CPOS )
      ENDDO
      CALL FIO_WRITE( LOG_UNIT, LINE(1:CPOS), STATUS )

*    Now compile record itself
      CALL CHR_FILL( ' ', LINE )
      CPOS = 0
      CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>1: ', LINE, CPOS )
      COLUMN = ' '
      CALL CHR_ITOC( EMLT_OBSNUM, COLUMN, CLEN )
      CALL CHR_PUTC( COLUMN, LINE, CPOS )
      CALL CHR_PUTC( ' ', LINE, CPOS )
      COLUMN = ' '
      CALL CHR_RTOC( EMLT_STIME, COLUMN, CLEN )
      CALL CHR_PUTC( COLUMN, LINE, CPOS )
      CALL CHR_PUTC( ' ', LINE, CPOS )
      COLUMN = ' '
      CALL CHR_RTOC( EMLT_GANGLE, COLUMN, CLEN )
      CALL CHR_PUTC( COLUMN, LINE, CPOS )
      CALL CHR_PUTC( ' ', LINE, CPOS )
      COLUMN = ' '
      CALL CHR_RTOC( EMLT_SSA, COLUMN, CLEN )
      CALL CHR_PUTC( COLUMN, LINE, CPOS )
      CALL CHR_PUTC( ' ', LINE, CPOS )
      COLUMN = ' '
      CALL CHR_RTOC( SLIT_ANGLE, COLUMN, CLEN )
      CALL CHR_PUTC( COLUMN, LINE, CPOS )
      CALL CHR_PUTC( ' ', LINE, CPOS )
      COLUMN = ' '
      CALL CHR_RTOC( EMLT_DFOCUS, COLUMN, CLEN )
      CALL CHR_PUTC( COLUMN, LINE, CPOS )
      CALL CHR_PUTC( ' ', LINE, CPOS )
      COLUMN = ' '
      CALL CHR_RTOC( XCEN, COLUMN, CLEN )
      CALL CHR_PUTC( COLUMN, LINE, CPOS )
      CALL CHR_PUTC( ' ', LINE, CPOS )
      COLUMN = ' '
      CALL CHR_RTOC( PEAK, COLUMN, CLEN )
      CALL CHR_PUTC( COLUMN, LINE, CPOS )
      CALL CHR_PUTC( ' ', LINE, CPOS )
      COLUMN = ' '
      CALL CHR_RTOC( XFWHM, COLUMN, CLEN  )
      CALL CHR_PUTC( COLUMN, LINE, CPOS )
      CALL CHR_PUTC( ' ', LINE, CPOS )

*    Write the record to the log file
      CALL FIO_WRITE( LOG_UNIT, LINE(1:CPOS), STATUS )
      END
