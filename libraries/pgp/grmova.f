      SUBROUTINE GRMOVA(X,Y)
*+
*
*     - - - - - - - -
*       G R M O V A      (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Sets new current position
*
*   Given
*      X        r     New position (world coordinates) X
*      Y        r     New position (world coordinates) Y
*
*   Read from COMMON
*      GRCIDE   i     Current device
*
*   Written to COMMON
*      GRXPRE   i()   Current point X
*      GRYPRE   i()   Current point Y
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE
      REAL X,Y
      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRMOVA - No PGPLOT device open',
     :   GRNODO)
      ELSE

*   Convert position to absolute coordinates
         CALL GRTXY0(.FALSE.,X,Y,GRXPRE(GRCIDE),GRYPRE(GRCIDE))
      END IF

      END
