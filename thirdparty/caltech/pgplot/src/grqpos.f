C*GRQPOS -- return current pen position (absolute, world coordinates)
C+
      SUBROUTINE GRQPOS(X,Y)
C
C GRQPOS: returns the current pen position in absolute, world
C coordinates.
C
C Arguments:
C
C X, Y (real, output): world coordinates of the pen position.
C--
C  1-Mar-1991 - new routine  [JM].
C-----------------------------------------------------------------------
      REAL     X,Y
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.GE.1) THEN
          X = (GRXPRE(GRCIDE) - GRXORG(GRCIDE)) / GRXSCL(GRCIDE)
          Y = (GRYPRE(GRCIDE) - GRYORG(GRCIDE)) / GRYSCL(GRCIDE)
      END IF
      END
