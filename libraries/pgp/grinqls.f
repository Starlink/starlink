      SUBROUTINE GRQLS(IS)
*+
*
*     - - - - - - -
*       G R Q L S    (GKS emulation of GRPCKG)
*     - - - - - - -
*
*   Returns the current line style.
*
*   Returned
*      IS       i     Line style
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*      GRSTYL  i()   Current line style
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INTEGER IS

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO','GRQLS - No PGPLOT device open',
     :   GRNODO)
         IS = 1
      ELSE
         IS = GRSTYL(GRCIDE)
      END IF

      END
