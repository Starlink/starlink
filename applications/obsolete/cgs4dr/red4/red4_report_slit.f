*+  RED4_REPORT_SLIT - Report slit info to the user.
      SUBROUTINE RED4_REPORT_SLIT( STATUS )
*    Description :
*     This routine reports information about the CGS4 slit derived
*     from information extracted from two EMLT.LIS files to the user.
*     Currently the offsets between two positions in an emission line
*     are reported, together with the slit angle calculated by the
*     CALC_SLIT_ANGLE action.
*    Invocation :
*     CALL RED4_REPORT_SLIT( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     15-May-1990: Original version, copied from RED4_LOG_SLIT.      (SMB)
*      2-Jul-1990: Made to give a warning if the slit angle is
*                  not within an acceptable range.                   (SMB)
*     24-Aug-1992: Correct error reporting                           (PND)
*     19-Jan-1995: Port to Unix                                      (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS         ! Global status
*    Global variables :
      INCLUDE 'RED4_ENG.INC' ! RED4 engineering common block
*    Local variables :
      REAL
     :  XDIFF, YDIFF,
     :  DIFF,           ! The different between SLIT_ANGLE and zero
     :  DIFF180         ! The different between SLIT_ANGLE and 180.0
*-

*    Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check there is a valid slit angle available
      IF ( .NOT. SLIT_ANGLE_VALID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REPORT_SLIT: There is no valid slit angle!', STATUS )
      ENDIF

*    Obtain the parameters to be written to the log file
      CALL PAR_GET0R( 'YCEN', YCEN, STATUS )
      CALL PAR_GET0R( 'YCEN_P', YCEN_P, STATUS )

*    Calculate the differences between the centres
      XDIFF = XCEN - XCEN_P
      YDIFF = YCEN - YCEN_P

*    Report the information
      CALL MSG_SETR( 'XDIFF', XDIFF )
      CALL MSG_SETR( 'YDIFF', YDIFF )
      CALL MSG_SETR( 'SLIT_ANGLE', SLIT_ANGLE )
      CALL MSG_OUT( ' ', 'Y difference = ^YDIFF; '/
     :   /'X difference = ^XDIFF; Slit angle = '/
     :   /'^SLIT_ANGLE degrees', STATUS )

*    Add a warning to the report if this slit angle is not acceptable.
      DIFF = ABS( SLIT_ANGLE )
      DIFF180 = ABS( DIFF - 180.0 )
      IF ( DIFF.GT.MAX_SLIT_ANGLE .AND. DIFF180.GT.MAX_SLIT_ANGLE ) THEN
         CALL MSG_OUT( ' ', '**** WARNING - This slit angle is NOT acceptable ****', STATUS )
      ENDIF
      END
