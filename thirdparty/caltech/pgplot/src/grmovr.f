
C*GRMOVR -- move pen (relative, world coordinates)
C+
      SUBROUTINE GRMOVR (DX,DY)
C
C GRPCKG: move the pen through a specified displacement.
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
          GRXPRE(GRCIDE) = GRXPRE(GRCIDE) + DX*GRXSCL(GRCIDE)
          GRYPRE(GRCIDE) = GRYPRE(GRCIDE) + DY*GRYSCL(GRCIDE)
      END IF
      END
