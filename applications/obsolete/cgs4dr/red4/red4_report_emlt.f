*+  RED4_REPORT_EMLT - Report EMLT info to the user.
      SUBROUTINE RED4_REPORT_EMLT( STATUS )
*    Description :
*     This routine reports the information obtained from the EMLT.LIS
*     to the user, using MSG_OUTs.
*    Invocation :
*     CALL RED4_REPORT_EMLT( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly  (JACH.HAWAII.EDU::PND)
*    History :
*     15-May-1990: Original version, copied from RED4_LOG_EMLT.     (SMB)
*      3-Jul-1990: Modified to check EMLT_VALID flag.               (SMB)
*     24-Aug-1992: Correct error reporting                          (PND)
*     19-Jan-1995: Port to Unix                                     (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS         ! Global status
*    Global variables :
      INCLUDE 'RED4_ENG.INC' ! RED4 common block
*-

*    Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that valid EMLT information has been obtained
      IF ( .NOT. EMLT_VALID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REPORT_EMLT: No valid EMLT info - use READ_EMLT first!', STATUS )
      ENDIF

*    Display the information
      CALL MSG_SETR( 'YCEN', YCEN )
      CALL MSG_OUT( ' ', 'Line at Y position = ^YCEN', STATUS )
      CALL MSG_SETR( 'BCEN', BCEN )
      CALL MSG_SETR( 'XCEN', XCEN )
      CALL MSG_SETR( 'BFWHM', BFWHM )
      CALL MSG_SETR( 'XFWHM', XFWHM )
      CALL MSG_OUT( ' ', 'X centroid = ^XCEN (^BCEN bins); '/
     :   /' FWHM = ^XFWHM (^BFWHM bins)', STATUS )
      CALL MSG_SETR( 'STRENGTH', STRENGTH )
      CALL MSG_SETR( 'PEAK', PEAK )
      CALL MSG_OUT( ' ', 'Integrated strength = ^STRENGTH; '/
     :   /'Peak height = ^PEAK', STATUS )
      CALL MSG_SETR( 'MEANBFWHM', MEANBFWHM )
      CALL MSG_SETR( 'MEANXFWHM', MEANXFWHM )
      CALL MSG_OUT( ' ', 'Mean FWHM of ALL lines = '/
     :   /'^MEANXFWHM (^MEANBFWHM bins)', STATUS )
      END
