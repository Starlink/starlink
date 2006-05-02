      SUBROUTINE gns_1TERMG(STATUS)
*+
*  Name:
*     gns_1TERMG
*
*  Purpose:
*     Close the GNS system for the GKS package
*
*  Invocation:
*     CALL GNS_1TERMG(STATUS)
*
*  Description:
*     The GKS devices data file is closed
*
*  Arguments:
*     STATUS = INTEGER (Given & Returned)

*  Authors:
*     DLT: D L Terrett (Starlink)

*  History:
*     16-MAY-1989 (DLT):
*        Original.
*-
      IMPLICIT NONE

      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'

      INTEGER STATUS

      LOGICAL OPEN
      
      INQUIRE ( UNIT = LUNGNS, OPENED = OPEN)
      IF (OPEN) CLOSE (UNIT=LUNGNS)
      
      END 
