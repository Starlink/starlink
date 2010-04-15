      SUBROUTINE GRPAGE
*+
*     - - - - - - - -
*       G R P A G E    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Clears the currently selected device unless the device was not
*   opened by GRPCKG
*
*   Read from COMMON
*      GRCIDE   i     Current GRPCKG device id
*      GRWKID   i()   Workstation id
*      GRWSOP   i()   W/S opened by GRPCKG
*
*   Constants from GKS_PAR
*      GCONDI  i     Clear conditionally
*
*   Externals
*      GCLRWK GRTERM  ERR_REP
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'GKS_PAR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRPAGE - No PGPLOT device open',
     :   GRNODO)
      ELSE
         CALL GRTERM
         IF (GRWSOP(GRCIDE)) CALL GCLRWK(GRWKID(GRCIDE),GCONDI)
      END IF
      END
