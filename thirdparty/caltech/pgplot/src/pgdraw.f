C*PGDRAW -- draw a line from the current pen position to a point
C%void cpgdraw(float x, float y);
C+
      SUBROUTINE PGDRAW (X, Y)
      REAL X, Y
C
C Draw a line from the current pen position to the point
C with world-coordinates (X,Y). The line is clipped at the edge of the
C current window. The new pen position is (X,Y) in world coordinates.
C
C Arguments:
C  X      (input)  : world x-coordinate of the end point of the line.
C  Y      (input)  : world y-coordinate of the end point of the line.
C--
C 27-Nov-1986
C-----------------------------------------------------------------------
      CALL PGBBUF
      CALL GRLINA(X,Y)
      CALL PGEBUF
      END
