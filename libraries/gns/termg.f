      SUBROUTINE gns_1TERMG(STATUS)
*+
*   Routine:
*
*      gns_1TERMG
*
*   Function:
*
*      Close the GNS system for the GKS package
*
*   Call:
*
*      CALL GNS_1TERMG(STATUS)
*
*   Description:
*
*      The GKS devices data file is closed
*
*   Arguments:
*
*      Modified:
*       
*         STATUS  (INTEGER)
*                        Inherited status
*       
*+
*   D L Terrett   16-MAY-1989 

      IMPLICIT NONE

      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'

      INTEGER STATUS

      LOGICAL OPEN
      
      INQUIRE ( UNIT = LUNGNS, OPENED = OPEN)
      IF (OPEN) CLOSE (UNIT=LUNGNS)
      
      END 
