*+  P4_SAVE - Save current configuration to file.
      SUBROUTINE P4_SAVE( STATUS )
*    Description :
*     This routine saves the plotting configuration to a file.
*    Invocation :
*     CALL P4_SAVE( STATUS )
*    Authors :
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*      7-Aug-1994: Original version.                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS          ! Global status
*    Local variables :
      INTEGER PORT            ! The port number to save
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Update the common block
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      CALL PAR_CANCL( 'PORT', STATUS )
      CALL P4_READ_NB( PORT, STATUS )

*   Write the current configuration to the specified file
      CALL P4_WRITE_CONFIG( PORT, STATUS )

      END
