
C*GRLINR -- draw a line (relative, world coordinates)
C+
      SUBROUTINE GRLINR (DX,DY)
C
C GRPCKG: draw a line from the current position by a specified
C relative displacement.
C
C Arguments:
C
C DX, DY (real, input): the displacement in world coordinates: the pen
C       position is incremented by DX in x and DY in y.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     DX,DY
C
      IF (GRCIDE.GE.1) THEN
          CALL GRLIN0( DX * GRXSCL(GRCIDE) + GRXPRE(GRCIDE),
     1                 DY * GRYSCL(GRCIDE) + GRYPRE(GRCIDE) )
      END IF
      END
