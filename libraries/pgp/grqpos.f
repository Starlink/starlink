      SUBROUTINE GRQPOS(X,Y)
*+
*
*     - - - - - - - -
*       G R Q P O S      (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Returns the current position in world coordinates
*
*   Returned
*      X        r     Current position (world coordinates) X
*      Y        r     Current position (world coordinates) Y
*
*   Read from COMMON
*      GRCIDE   i     Current device
*      GRXPRE   i()   Current point X
*      GRYPRE   i()   Current point Y
*      GRXSCL   r()   X scale
*      GRYSCL   r()   Y scale
*      GRXORG   r()   X origin
*      GRYORG   r()   Y origin
*
*   D.L.Terrett  Starlink  Mar 1991
*+
      IMPLICIT NONE
      REAL X,Y
      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRQPOS - No PGPLOT device open',
     :   GRNODO)
      ELSE

*   Convert position to world coordinates
         X = (GRXPRE(GRCIDE) - GRXORG(GRCIDE)) / GRXSCL(GRCIDE)
         Y = (GRYPRE(GRCIDE) - GRYORG(GRCIDE)) / GRYSCL(GRCIDE)
      END IF

      END
