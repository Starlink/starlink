      SUBROUTINE GRCLWK(IWKID)
*+
*
*     - - - - - - - -
*       G R C L W K     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Deactivate and close the specified device then, if there are no
*   other devices open, close GKS
*
*   Constants from GKS_PAR
*      GGKOP     i    Operating state - GKS open
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE
      INCLUDE 'GKS_PAR'


      INTEGER IWKID, ISTATE

*   Deactivate the workstation
      CALL GDAWK(IWKID)

*   Close it
      CALL GCLWK(IWKID)

*   If no more workstations are open then close GKS
      CALL GQOPS(ISTATE)
      IF (ISTATE.EQ.GGKOP) CALL GCLKS

      END
