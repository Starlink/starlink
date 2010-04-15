*+  RED3 - The CGS3 data reduction A-task monolith.
      SUBROUTINE RED3 (STATUS)
*    Description :
*     This is the main program for the RED3 A-task monolith, which
*     is a very much cut down version of the CGS4 monolith.
*    Invocation :
*     Invoked by the a-task environment
*    Parameters :
*    Method :
*     <description of how the task works>
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     July 1990:  Original version                             (JFL)
*     24-Sep-90: Addition of SCALE (JAC::AB)
*     25-Sep-90: Add OLD_CGS3_41 and OLDCGS3_42 (JAC::AB)
*     28-Sep-90: Add CGS3_DET (JAC::AB)
*      7-Nov-90: Add CGS2FF (JAC::AB)
*      7-Jun-91: Removed CGS2FF, ADJOIN, RCGS2 - now in Figaro (JAC::AB)
*      7-Jun-91: Removed OLD_CGS3_41, OLD_CGS3_42 - unlikely to be
*                required any more (JAC::AB)
*     26-Sep-91: Removed many functions now in Figaro V3.0 (JAC::AB)
*     18-Nov-91: Add CGS3_43 (JAC::AB)
*     18-Nov-91: Put RED4_ADJOIN and RED4_EXTRACT back as the Figaro ones
*                still have problems! (JAC::AB)
*     19-Feb-93: Add trap for arithmetic errors (JAC::AB)
*     13-Nov-93: Add cgs3_phred (JAC::AB)
*     30-Nov-95: remove adamdefns, adamerrs for unix porting (KK)
*     20-Dec-95: take NAME out of call to RED3...
*     27-Feb-96: changed subroutine calls from RED4_ to RED3_
*      4-Mar-96: add cgs3pol
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'PRM_PAR'
*    Import:
      CHARACTER*(PAR__SZNAM) NAME
*    Status :
      INTEGER STATUS
*    External references :
*    Local Constants :
*    Local variables :
*    Global variables :
*    Internal References :
*    Local data :
*-

      IF( STATUS .EQ. SAI__OK ) THEN

        CALL TASK_GET_NAME ( NAME, STATUS )

         IF ( NAME .EQ. 'BLACK_BODY') THEN

*  Calculate a black-body spectrum on a template wavelength axis

            CALL RED3_BLACK (STATUS)

         ELSE IF ( NAME .EQ. 'CGS3_41' ) THEN

*  Extract the data from a CGS3 hypercube

            CALL RED3_CGS3_41 (STATUS)

         ELSE IF ( NAME .EQ. 'CGS3_42') THEN

*  Convert CGS3 data hyper-cube into image of spectra versus cycle number,
*  of for polarimetry spectra versus plate position.

            CALL RED3_CGS3_42 (STATUS)

         ELSE IF ( NAME .EQ. 'CGS3_43') THEN

*  Convert CGS3 data hyper-cube into cube of spectra versus cycle number,
*  for polarimetry only

            CALL RED3_CGS3_43 (STATUS)

         ELSE IF ( NAME .EQ. 'CGS3_BAD_CYCLE' ) THEN

*  Set bad quality on duff cycles in CGS3 spectrum versus cycle image

            CALL RED3_CGS3_SETBAD (STATUS)

         ELSE IF ( NAME .EQ. 'CGS3_DET' ) THEN

*  extract detector calibration spectrum from raw calibration data

            CALL RED3_CGS3_DET (STATUS)

         ELSE IF ( NAME .EQ. 'EXTRACT3') THEN

*  Use RED3s extract to extract data from an "image"

            CALL RED3_EXTRACT (STATUS)

         ELSE IF ( NAME .EQ. 'ADJOIN3') THEN

*  Use RED3's ADJOIN

            CALL RED3_ADJOIN (STATUS)

         ELSE IF ( NAME .EQ. 'SCALE' ) THEN

*  Scale an input image/spectrum

            CALL RED3_SCALE (STATUS)

*  Extract the scans from the latest run and treat as photometry

         ELSE IF ( NAME .EQ. 'CGS3_PHRED' ) THEN

            CALL RED3_CGS3_PHRED (STATUS)

*  Reduce polarimetry data

         ELSE IF ( NAME .EQ. 'CGS3POL' ) THEN

            CALL RED3_CGS3POL (STATUS)

         ELSE

*  Invalid action name

            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_OUT( ' ', 'RED3 - Invalid action: ^NAME', STATUS )

         END IF

      END IF

      END
