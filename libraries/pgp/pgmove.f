C*PGMOVE -- move pen (change current pen position)
C%void cpgmove(float x, float y);
C+
      SUBROUTINE PGMOVE (X, Y)
      REAL X, Y
C
C Primitive routine to move the "pen" to the point with world
C coordinates (X,Y). No line is drawn.
C
C Arguments:
C  X      (input)  : world x-coordinate of the new pen position.
C  Y      (input)  : world y-coordinate of the new pen position.
C--
C (29-Dec-1983)
C-----------------------------------------------------------------------
      CALL GRMOVA(X,Y)
      END
