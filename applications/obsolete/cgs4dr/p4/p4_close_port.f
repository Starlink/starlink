*+  P4_CLOSE_PORT - Checks the port for an open device
      SUBROUTINE P4_CLOSE_PORT( STATUS )
*    Invocation :
*     CALL P4_CLOSE_PORT( STATUS )
*    Authors :
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     15-Dec-1994: Original Unix version                     (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS           ! Global status
*    Global variables :
      INCLUDE 'P4COM.INC'      ! P4 common block
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Close PGPLOT
      CALL PGEND
      CALL CHR_FILL( ' ', CURRENT_DEVICE )
      CURRENT_DEVICE = 'NONE/CLOSED'

*    Exit
      END
