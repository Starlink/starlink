*+  RED4_READ_EMLT - Extract information from EMLT.LIS file
      SUBROUTINE RED4_READ_EMLT( STATUS )
*    Description :
*     This routine extracts information regarding the positions
*     and widths of the lines detected by the Figaro EMLT function
*     and stored in the file EMLT.LIS. The information is written
*     to ADAM parameters, so that it may be read by other tasks and
*     ICL procedures.
*    Invocation :
*     CALL RED4_READ_EMLT( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     11-May-1990: Original version.                           (SMB)
*     14-May-1990: DELETE_EMLT and SLIT_ANGLE_VALID included.  (SMB)
*      3-Jul-1990: Modified to set the EMLT_VALID flag.        (SMB)
*     24-Aug-1992: Correct error reporting                     (PND)
*     19-Jan-1995: Ported to Unix                              (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS             ! Global status
*    Global variables :
      INCLUDE 'RED4_ENG.INC'     ! RED4 common block
*    Local variables :
      REAL XMIN, XMAX
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Unset the EMLT_VALID flag, so if an error occurs it will be left .FALSE.
      EMLT_VALID = .FALSE.

*    Get the XMIN and XMAX parameters
      CALL PAR_GET0R( 'XMIN', XMIN, STATUS )
      CALL PAR_GET0R( 'XMAX', XMAX, STATUS )

*    If there has been a previous measurement, remember the previous values of the parameters
      IF ( NMEAS .GT. 0 ) THEN
         BCEN_P = BCEN
         XCEN_P = XCEN
         BFWHM_P = BFWHM
         XFWHM_P = XFWHM
         STRENGTH_P = STRENGTH
         PEAK_P = PEAK
         MEANBFWHM_P = MEANBFWHM
         MEANXFWHM_P = MEANXFWHM
      ENDIF

*    Read the information from the $CGS4_ENG/emlt.lis file
      CALL RED4_READ_EMLT2( XMIN, XMAX, BCEN, XCEN, BFWHM, XFWHM, STRENGTH, PEAK, MEANBFWHM, MEANXFWHM, STATUS )

*    If this has worked, adjust NMEAS to record whether
*    an EMLT.LIS file has been read an odd or even number
*    of times. Also set EMLT_VALID and reset SLIT_ANGLE_VALID
*    to indicate there is no longer a valid slit angle.
*    Otherwise report an error.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NMEAS .EQ. 1 ) THEN
            NMEAS = 2
         ELSE
            NMEAS = 1
         ENDIF
         EMLT_VALID = .TRUE.
         SLIT_ANGLE_VALID = .FALSE.
      END IF
      END
