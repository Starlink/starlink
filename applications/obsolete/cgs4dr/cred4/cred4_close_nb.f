*+  CRED4_CLOSE_NB - Save the CRED4 noticeboard to a file
      SUBROUTINE CRED4_CLOSE_NB( STATUS )
*    Description :
*     This routine saves the contents of the CRED4 noticeboard
*     to the file previously created or restored, and then sets
*     a flag to show the noticeboard has been closed. The routine
*     is usually executed just before shutting down the data reduction
*     system.
*    Invocation :
*     CALL CRED4_CLOSE_NB( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     21-Jun-1990: Original version.                           (SMB)
*     10-Sep-1990: Renamed to CRED4_CLOSE_NB and setting of
*                  SEQUENCE_SETUP added. Message removed.      (SMB)
*     15-Jan-1993: Re-written to store NB config in par file.  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS                   ! Global status
*    Global variables :
      INCLUDE 'CRED4COM.INC'           ! CRED4 common block
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Read the current noticeboard values into the common block
      CALL CRED4_READ_NB( STATUS )

*   Write the common block values out to the interface (parameter) file
      CALL CRED4_WRITE_PARAMETERS( STATUS )

*   Unmap (lose) the noticeboard
      CALL NBS_LOSE_NOTICEBOARD( NB_TOPID, 'FORCE', STATUS )

*   Set the reduction sequence to false
      SEQUENCE_SETUP = .FALSE.

      END
