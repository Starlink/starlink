*+  RED4_LOG_SLIT - Write time-stamped slit info to the log file
      SUBROUTINE RED4_LOG_SLIT( STATUS )
*    Description :
*     This routine writes time-stamped records to the engineering
*     log file recording information about the CGS4 slit derived
*     from information extracted from two EMLT.LIS files.
*     Currently the offsets between two positions in an emission line
*     are recorded, together with the slit angle calculated by the
*     CALC_SLIT_ANGLE action.
*    Invocation :
*     CALL RED4_LOG_SLIT( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
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
      CHARACTER*132 LINE     ! Line to be written to the log file
      CHARACTER*80 STAMP     ! Time stamp
      REAL
     :  XDIFF, YDIFF,
     :  DIFF,                ! The different between SLIT_ANGLE and zero
     :  DIFF180              ! The different between SLIT_ANGLE and 180.0
      INTEGER
     :  CPOS                 ! Character position
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that the log file has been opened
      IF ( .NOT. LOG_OPEN ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_LOG_SLIT: Engineering log file not open!', STATUS )
      ENDIF

*    Check there is a valid slit angle available.
      IF ( .NOT. SLIT_ANGLE_VALID ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_LOG_SLIT: There is no valid slit angle!', STATUS )
      ENDIF

*    Calculate the differences between the centres
      CALL PAR_GET0R( 'YCEN', YCEN, STATUS )
      CALL PAR_GET0R( 'YCEN_P', YCEN_P, STATUS )
      XDIFF = XCEN - XCEN_P
      YDIFF = YCEN - YCEN_P

*    Obtain a time stamp
      CALL RED4_TIME_STAMP( STAMP, STATUS )

*    Put together a string describing the line shift and the slit angle to be written to the file
      CALL CHR_FILL( ' ', LINE )
      CPOS = 0
      CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LINE, CPOS )
      CALL CHR_PUTC( 'Y difference = ', LINE, CPOS )
      CALL CHR_PUTR( YDIFF, LINE, CPOS )
      CALL CHR_PUTC( '; X difference = ', LINE, CPOS )
      CALL CHR_PUTR( XDIFF, LINE, CPOS )
      CALL CHR_PUTC( '; so the slit angle = ', LINE, CPOS )
      CALL CHR_PUTR( SLIT_ANGLE, LINE, CPOS )
      CALL CHR_PUTC( ' degrees', LINE, CPOS )
      CALL FIO_WRITE( LOG_UNIT, LINE(1:CPOS), STATUS )

*    Add a warning to the log file if this slit angle is not acceptable
      DIFF = ABS( SLIT_ANGLE )
      DIFF180 = ABS( DIFF - 180.0 )
      IF ( DIFF.GT.MAX_SLIT_ANGLE .AND. DIFF180.GT.MAX_SLIT_ANGLE ) THEN
         CALL CHR_FILL( ' ', LINE )
         CPOS = 0
         CALL CHR_PUTC( STAMP(1:CHR_LEN(STAMP))//'>! ', LINE, CPOS )
         CALL CHR_PUTC( '**** W A R N I N G  - This slit angle is NOT acceptable ****', LINE, CPOS )
         CALL FIO_WRITE( LOG_UNIT, LINE(1:CPOS), STATUS )
      ENDIF
      END
