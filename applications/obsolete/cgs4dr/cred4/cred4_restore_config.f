*+  CRED4_RESTORE_CONFIG - Restore current configuration from file.
      SUBROUTINE CRED4_RESTORE_CONFIG( STATUS )
*    Description :
*     This routine reads the data reduction configuration stored in a
*     file and copies it to the common block variables.
*     The file is assumed to be in the CGS4_CONFIG directory by default.
*    Invocation :
*     CALL CRED4_RESTORE_CONFIG( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     10-Sep-1990: Original version.                    (SMB)
*     11-Sep-1990: Test on SEQUENCE_SETUP included.     (SMB)
*     15-Jan-1992: Need to read the nb for new configs  (PND)
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

*      Restore the configuration from the specified file to the parameter file
         CALL CRED4_READ_CONFIG( STATUS )

*      Write the parameters to the common block variables
         CALL CRED4_READ_PARAMETERS( STATUS )

*      Write common block variables to the noticeboard
         CALL CRED4_WRITE_NB( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CRED4_RESTORE_CONFIG: '/
     :      /'Data reduction sequence not set up', STATUS )
      ENDIF

      END
