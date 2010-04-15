*+  P4_BAR - Plot a colour scale bar
      SUBROUTINE P4_BAR( STATUS )
*    Description :
*     This routine plots a colour scale bar on the right hand side.
*    Invocation :
*     CALL P4_BAR( STATUS )
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S. M. Beard (REVAD::SMB)
*     P. N. Daly (JACH::PND)
*    History :
*          1989 ?: Original version.                             (JFL)
*     24-Oct-1989: History added. Extra status checks included.  (SMB)
*     25-Oct-1989: Setting of status to ACT_END removed, so that
*                  P4_ENDACTION may be used.                     (SMB)
*     28-Aug-1990: Description added.                            (SMB)
*      4-Aug-1994: Converted to I-task for Unix port             (PND)
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

*    Return immediately if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the port number and read the noticeboard
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      CALL P4_READ_NB( PORT, STATUS )

*    Check that a plot device is open
      CALL P4_CHECK_PORT( PORT, STATUS )

*    Plot the bar
      CALL P4_PLOTBAR( PORT, STATUS )
      END
