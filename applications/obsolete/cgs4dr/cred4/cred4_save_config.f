*+  CRED4_SAVE_CONFIG - Save current configuration to file.
      SUBROUTINE CRED4_SAVE_CONFIG( STATUS )
*    Description :
*     This routine reads the data reduction configuration from the
*     noticeboard and then saves it to a given file. The file is assumed
*     to be in the CGS4_CONFIG directory by default.
*    Invocation :
*     CALL CRED4_SAVE_CONFIG( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     10-Sep-1990: Original version.                    (SMB)
*     11-Sep-1990: Test on SEQUENCE_SETUP included.     (SMB)
*     12-Feb-1993: Conform to error strategy            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS             ! Global status
*    Global variables :
      INCLUDE 'CRED4COM.INC'     ! CRED4 common block
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Ensure that a data reduction sequence has been set up
      IF ( SEQUENCE_SETUP ) THEN

*      Read the noticeboard, to ensure any recent changes are incorporated.
         CALL CRED4_READ_NB( STATUS )

*      Write the current configuration to the specified file.
         CALL CRED4_WRITE_CONFIG( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CRED4_SAVE_CONFIG: '/
     :     /'Data reduction sequence not set up', STATUS )
         CALL ERR_REP( ' ', 'CRED4_SAVE_CONFIG: '/
     :     /'NB must be opened before DR params can be saved', STATUS )
      END IF

      END
