      SUBROUTINE GRQCI(ICOL)
*+
*
*     - - - - - - -
*       G R Q C I    (GKS emulation of GRPCKG)
*     - - - - - - -
*
*   Returns the current colour.
*
*   Returned
*      ICOL     i     Colour index
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*      GRCCOL   i()   Current colour
*
*   D.L.Terrett  Starlink  Mar 1987
*+
      IMPLICIT NONE

      INTEGER ICOL

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRQCI - No PGPLOT device open',
     :   GRNODO)
         ICOL = 1
      ELSE
         ICOL = GRCCOL(GRCIDE)
      END IF

      END
