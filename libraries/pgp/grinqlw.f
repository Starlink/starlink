      SUBROUTINE GRQLW(ILW)
*+
*
*     - - - - - - -
*       G R Q L W    (GKS emulation of GRPCKG)
*     - - - - - - -
*
*   Return the current line width.
*
*   Returned
*      ILW      i     Line width
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*      GRWIDT   i()   Current line width
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INTEGER ILW

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRQLW - No PGPLOT device open',
     :   GRNODO)
         ILW = 1
      ELSE
         ILW = ABS(GRWIDT(GRCIDE))
      END IF

      END
