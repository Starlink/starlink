*+  P4_CURSOR - Position cursor, return co-ords and keystroke,
      SUBROUTINE P4_CURSOR( STATUS )
*    Invocation :
*     CALL P4_CURSOR( STATUS )
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S. M. Beard (REVAD::SMB)
*     P. N. Daly (JACH::PND)
*    History :
*     1989:        Original version.                                (JFL)
*     24-Oct-1989: History added. Further status checks added.
*                  Superfluous call to DSA_CLOSE removed.           (SMB)
*     25-Oct-1989: Setting of status to ACT_END removed, so that
*                  P4_ENDACTION may be used.                        (SMB)
*      2-Nov-1989: Status check after PGPOINT improved. Warning
*                  issued if the cursor is outside the plot window. (SMB)
*     16-Feb-1990: CURSOR_STATUS parameter added, so that ICL
*                  procedures can stop if something has gone wrong. (SMB)
*     16-Feb-1990: Bug fix. Check on window limits was incorrect.   (SMB)
*     20-Jul-1990: COLUMNS added for HISTOGRAM.                     (SMB)
*     10-Feb-1991: Bug which caused this routine to crash if called
*                  without a device open fixed.                     (SMB)
*     18-Feb-1993: Tidy code                                        (PND)
*     12-Aug-1994: Major modifications for Unix port                (PND)
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

*    Get the port and read the noticeboard
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      CALL P4_READ_NB( PORT, STATUS )

*    Check that plot device is open
      CALL P4_CHECK_PORT( PORT, STATUS )

*    Get the cursor value
      CALL P4_GET_CURSOR( PORT, STATUS )

      END
