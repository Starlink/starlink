C*PGLINE -- draw a polyline (curve defined by line-segments)
C%void cpgline(int n, const float *xpts, const float *ypts);
C+
      SUBROUTINE PGLINE (N, XPTS, YPTS)
      INTEGER  N
      REAL     XPTS(*), YPTS(*)
C
C Primitive routine to draw a Polyline. A polyline is one or more
C connected straight-line segments.  The polyline is drawn using
C the current setting of attributes color-index, line-style, and
C line-width. The polyline is clipped at the edge of the window.
C
C Arguments:
C  N      (input)  : number of points defining the line; the line
C                    consists of (N-1) straight-line segments.
C                    N should be greater than 1 (if it is 1 or less,
C                    nothing will be drawn).
C  XPTS   (input)  : world x-coordinates of the points.
C  YPTS   (input)  : world y-coordinates of the points.
C
C The dimension of arrays X and Y must be greater than or equal to N.
C The "pen position" is changed to (X(N),Y(N)) in world coordinates
C (if N > 1).
C--
C 27-Nov-1986
C-----------------------------------------------------------------------
      INTEGER  I
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGLINE')) RETURN
      IF (N.LT.2) RETURN
C
      CALL PGBBUF
      CALL GRMOVA(XPTS(1),YPTS(1))
      DO 10 I=2,N
         CALL GRLINA(XPTS(I),YPTS(I))
 10   CONTINUE
      CALL PGEBUF
      END
