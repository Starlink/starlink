*+  P4_CLEAR - Clear a subwindow on the device display surface
      SUBROUTINE P4_CLEAR( STATUS )
*    Invocation :
*     CALL P4_CLEAR( STATUS )
*     P. N. Daly (JACH::PND)
*    History :
*     12-Aug-1994: Original Unix versionl                           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER PORT                            ! Port number
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get port number and clear it
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      CALL P4_CLEARPORT( PORT, STATUS )

      END
