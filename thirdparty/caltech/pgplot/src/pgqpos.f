C*PGQPOS -- inquire current pen position
C%void cpgqpos(float *x, float *y);
C+
      SUBROUTINE PGQPOS (X, Y)
      REAL X, Y
C
C Query the current "pen" position in world C coordinates (X,Y).
C
C Arguments:
C  X      (output)  : world x-coordinate of the pen position.
C  Y      (output)  : world y-coordinate of the pen position.
C--
C  1-Mar-1991 - new routine [JM].
C-----------------------------------------------------------------------
      CALL GRQPOS(X,Y)
      END
