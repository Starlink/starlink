C*PGRECT -- draw a rectangle, using fill-area attributes
C%void cpgrect(float x1, float x2, float y1, float y2);
C+
      SUBROUTINE PGRECT (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C This routine can be used instead of PGPOLY for the special case of
C drawing a rectangle aligned with the coordinate axes; only two
C vertices need be specified instead of four.  On most devices, it is
C faster to use PGRECT than PGPOLY for drawing rectangles.  The
C rectangle has vertices at (X1,Y1), (X1,Y2), (X2,Y2), and (X2,Y1).
C
C Arguments:
C  X1, X2 (input) : the horizontal range of the rectangle.
C  Y1, Y2 (input) : the vertical range of the rectangle.
C--
C 21-Nov-1986 - [TJP].
C 22-Mar-1988 - use GRRECT for fill [TJP].
C  6-Mar-1995 - add hatching (by calling PGHTCH) [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL XP(4), YP(4)
C
      CALL PGBBUF
C
C Outline only.
C
      IF (PGFAS(PGID).EQ.2) THEN
         CALL GRMOVA(X1,Y1)
         CALL GRLINA(X1,Y2)
         CALL GRLINA(X2,Y2)
         CALL GRLINA(X2,Y1)
         CALL GRLINA(X1,Y1)
C
C Hatching.
C
      ELSE IF (PGFAS(PGID).EQ.3 .OR. PGFAS(PGID).EQ.4) THEN
         XP(1) = X1
         XP(2) = X1
         XP(3) = X2
         XP(4) = X2
         YP(1) = Y1
         YP(2) = Y2
         YP(3) = Y2
         YP(4) = Y1
         CALL PGHTCH(4, XP, YP, 0.0)
         IF (PGFAS(PGID).EQ.4) CALL PGHTCH(4, XP, YP, 90.0)
C
C Solid fill.
C
      ELSE
          CALL GRRECT(X1,Y1,X2,Y2)
          CALL GRMOVA(X1,Y1)
      END IF
      CALL PGEBUF
      END
