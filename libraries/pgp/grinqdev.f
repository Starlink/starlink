      SUBROUTINE GRQDT(DEVICE)
*+
*     - - - - - - -
*       G R Q D T      (GKS emulation of GRPCKG)
*     - - - - - - -
*
*   Returns the name of the currently selected device (the name used in
*   the call to GROPEN).
*
*   Returned
*      DEVICE     c      Device name
*
*   Read from COMMON
*      GRCIDE     i      Current device identifier
*      GRNAME     c      The device name
*
*   D.L.Terrett  Starlink  Jul 1987
*+
      IMPLICIT NONE
      CHARACTER*(*) DEVICE

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO','GRQDT - No PGPLOT device open',
     :   GRNODO)
         DEVICE = ' '
      ELSE
         DEVICE = GRNAME(GRCIDE)
      END IF

      END
