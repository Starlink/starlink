      SUBROUTINE GRQFNT(IFONT)
*+
*
*     - - - - - - - -
*       G R Q F N T    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Return the current font.
*
*   Returned
*      IFONT    i     Font number
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*      GRCFNT   i()   Current font
*
*   D.L.Terrett  Starlink  Jul 1987
*+
      IMPLICIT NONE

      INTEGER IFONT

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRQFNT - No PGPLOT device open',
     :    GRNODO)
         IFONT = 1
      ELSE
         IFONT = GRCFNT(GRCIDE)
      END IF

      END
