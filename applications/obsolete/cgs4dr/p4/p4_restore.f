*+  P4_RESTORE - Restore current configuration from file.
      SUBROUTINE P4_RESTORE( STATUS )
*    Description :
*     This routine restores the plooting configuration from a file.
*    Invocation :
*     CALL P4_RESTORE( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*      7-Aug-1994: Original version.                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS             ! Global status
*    Local variables :
      INTEGER PORT               ! The port number
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Restore the configuration from the specified file to common block
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      CALL PAR_CANCL( 'PORT', STATUS )
      CALL P4_READ_CONFIG( PORT, STATUS )

*    Write common block variables to the noticeboard
      CALL P4_WRITE_NB( PORT, STATUS )

      END
