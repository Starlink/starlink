*+  RED4_READ_OBS - Read a reduced observation file and extract info
      SUBROUTINE RED4_READ_OBS( STATUS )
*    Description :
*     This routine extracts engineering information from a reduced
*     observation file and writes it to parameters.
*    Invocation :
*     CALL RED4_READ_OBS( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     29-Jun-1990: Original version.                           (SMB)
*      2-Jul-1990: OBSERVATION_VALID flag added.               (SMB)
*     10-Sep-1990: Modified to extract the information from a
*                  FITS structure within the observation file. (SMB)
*     24-Aug-1992: Correct error reporting                     (PND)
*     17-Feb-1993: Check the input file for directory spec     (PND)
*     26-May-1993: Remove RED4_REPORT_DSA_ERROR call           (PND)
*     19-Jan-1995: Port to Unix                                (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS               ! Global status
*    Global variables :
      INCLUDE 'RED4_ENG.INC'       ! RED4 engineering common block
*    Local variables :
      CHARACTER*255 OBSERVATION    ! Name of the reduced observation file
      CHARACTER*4 COMMENT          ! Dummy comment, used for DSA FITS routines.
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Unset the OBSERVATION_VALID flag, so it will be unset if an error occurs.
      OBSERVATION_VALID = .FALSE.

*    Obtain the name of the reduced observation file and check it's input
      CALL PAR_GET0C( 'OBSERVATION', OBSERVATION, STATUS )

*    Open DSA and open the reduced observation file.
      CALL DSA_OPEN( STATUS )
      CALL DSA_NAMED_INPUT( 'OBS', OBSERVATION, STATUS )

*    Obtain the values for the items in the data structure
*    which are of interest from the FITS structure in the reduced observation file.
      CALL DSA_GET_FITS_I( 'OBS', 'OBSNUM', 0, EMLT_OBSNUM, COMMENT, STATUS )
      CALL DSA_GET_FITS_F( 'OBS', 'GANGLE', 0, EMLT_GANGLE, COMMENT, STATUS )
      CALL DSA_GET_FITS_F( 'OBS', 'GLAMBDA', 0, EMLT_GWAVE, COMMENT, STATUS )
      CALL DSA_GET_FITS_I( 'OBS', 'GORDER', 0, EMLT_GORDER, COMMENT, STATUS )
      CALL DSA_GET_FITS_F( 'OBS', 'DFOCUS', 0, EMLT_DFOCUS,COMMENT, STATUS )
      CALL DSA_GET_FITS_F( 'OBS', 'SANGLE', 0, EMLT_SSA, COMMENT, STATUS )
      CALL DSA_GET_FITS_F( 'OBS', 'RUTSTART', 0, EMLT_STIME, COMMENT, STATUS )
      CALL DSA_GET_FITS_F( 'OBS', 'RUTEND', 0, EMLT_ETIME, COMMENT, STATUS )

*    If this has worked, set a flag to indicate the common block values are OK
      IF ( STATUS .EQ. SAI__OK ) THEN
         OBSERVATION_VALID = .TRUE.
      ELSE
         OBSERVATION_VALID = .FALSE.
         STATUS= SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_READ_OBS: Failed to write output parameters', STATUS )
      ENDIF
      CALL DSA_CLOSE( STATUS )
      END
