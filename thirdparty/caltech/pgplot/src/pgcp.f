C
      SUBROUTINE PGCP (K, X, Y, Z)
C
C PGPLOT (internal routine): Draw one contour segment (for use by
C PGCNSC).
C
C Arguments:
C
C K (input, integer): if K=0, move the pen to (X,Y); if K=1, draw
C       a line from the current position to (X,Y); otherwise
C       do nothing.
C X (input, real): X world-coordinate of end point.
C Y (input, real): Y world-coordinate of end point.
C Z (input, real): the value of the contour level, not used by PGCP at
C       the moment.
C
C (7-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      INTEGER  K
      REAL     X,XX,Y,YY,Z
C
      XX = TRANS(1) + TRANS(2)*X + TRANS(3)*Y
      YY = TRANS(4) + TRANS(5)*X + TRANS(6)*Y
      IF (K.EQ.1) THEN
          CALL GRLINA(XX,YY)
      ELSE IF (K.EQ.0) THEN
          CALL GRMOVA(XX,YY)
      END IF
      END
