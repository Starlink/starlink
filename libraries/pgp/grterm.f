      SUBROUTINE GRTERM
*+
*     - - - - - - - -
*       G R T E R M     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Update current workstation
*
*   Read from COMMON
*      GRCIDE    i      Current device
*
*   Constants from GKS_PAR
*      GPOSTP    i      Regeneration postponed
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'GKS_PAR'


      IF (GRCIDE.EQ.0) THEN
         CALL ERR_REP('GRNODO', 'GRTERM - No PGPLTO device open',
     :   GRNODO)
      ELSE

*     Flush line buffer
         CALL GRFLU0

*     Update workstation
         CALL GUWK(GRWKID(GRCIDE),GPOSTP)
      END IF
      END
