C*PGPOLY -- draw a polygon, using fill-area attributes
C%void cpgpoly(int n, const float *xpts, const float *ypts);
C+
      SUBROUTINE PGPOLY (N, XPTS, YPTS)
      INTEGER N
      REAL XPTS(*), YPTS(*)
C
C Fill-area primitive routine: shade the interior of a closed
C polygon in the current window.  The action of this routine depends
C on the setting of the Fill-Area Style attribute (see PGSFS).
C The polygon is clipped at the edge of the
C window. The pen position is changed to (XPTS(1),YPTS(1)) in world
C coordinates (if N > 1).  If the polygon is not convex, a point is
C assumed to lie inside the polygon if a straight line drawn to
C infinity intersects and odd number of the polygon's edges.
C
C Arguments:
C  N      (input)  : number of points defining the polygon; the
C                    line consists of N straight-line segments,
C                    joining points 1 to 2, 2 to 3,... N-1 to N, N to 1.
C                    N should be greater than 2 (if it is 2 or less,
C                    nothing will be drawn).
C  XPTS   (input)  : world x-coordinates of the vertices.
C  YPTS   (input)  : world y-coordinates of the vertices.
C                    Note: the dimension of arrays XPTS and YPTS must be
C                    greater than or equal to N.
C--
C 21-Nov-1983 - [TJP].
C 16-Jul-1984 - revised to shade polygon with GRFA [TJP].
C 21-Oct-1985 - test PGFAS [TJP].
C 25-Nov-1994 - implement clipping [TJP].
C 13-Jan-1994 - fix bug in clipping [TJP].
C  6-Mar-1995 - add support for fill styles 3 and 4 [TJP].
C 12-Sep-1995 - fix another bug in clipping [TJP].
C-----------------------------------------------------------------------
      INTEGER MAXOUT
      PARAMETER (MAXOUT=1000)
      LOGICAL CLIP
      INTEGER I, N1, N2, N3, N4
      REAL    QX(MAXOUT), QY(MAXOUT), RX(MAXOUT), RY(MAXOUT)
      REAL    XL, XH, YL, YH
      LOGICAL PGNOTO
      INCLUDE 'pgplot.inc'
C
      IF (PGNOTO('PGPOLY')) RETURN
      IF (N.LT.1) RETURN
C
C Outline style, or polygon of less than 3 vertices.
C
      IF (PGFAS(PGID).EQ.2 .OR. N.LT.3) THEN
         CALL PGBBUF
         CALL GRMOVA(XPTS(N),YPTS(N))
         DO 10 I=1,N
            CALL GRLINA(XPTS(I),YPTS(I))
 10      CONTINUE
C
C Hatched style.
C
      ELSE IF (PGFAS(PGID).EQ.3) THEN
         CALL PGBBUF
         CALL PGHTCH(N, XPTS, YPTS, 0.0)
      ELSE IF (PGFAS(PGID).EQ.4) THEN
         CALL PGBBUF
         CALL PGHTCH(N, XPTS, YPTS, 0.0)
         CALL PGHTCH(N, XPTS, YPTS, 90.0)
      ELSE
C     
C Test whether polygon lies completely in the window.
C     
         CLIP = .FALSE.
         XL = MIN(PGXBLC(PGID),PGXTRC(PGID))
         XH = MAX(PGXBLC(PGID),PGXTRC(PGID))
         YL = MIN(PGYBLC(PGID),PGYTRC(PGID))
         YH = MAX(PGYBLC(PGID),PGYTRC(PGID))
         DO 20 I=1,N
            IF (XPTS(I).LT.XL .OR. XPTS(I).GT.XH .OR.
     :           YPTS(I).LT.YL .OR. YPTS(I).GT.YH) THEN
               CLIP = .TRUE.
               GOTO 30
            END IF
 20      CONTINUE
 30      CONTINUE
C     
C Filled style, no clipping required.
C     
         CALL PGBBUF
         IF (.NOT.CLIP) THEN
            CALL GRFA(N,XPTS,YPTS)
C     
C Filled style, clipping required: the vertices of the clipped
C polygon are put in temporary arrays QX,QY, RX, RY.
C     
         ELSE
            CALL GRPOCL(N,  XPTS, YPTS, 1, XL, MAXOUT, N1, QX, QY)
            IF (N1.GT.MAXOUT) GOTO 40
            IF (N1.LT.3) GOTO 50
            CALL GRPOCL(N1, QX,   QY,   2, XH, MAXOUT, N2, RX, RY)
            IF (N2.GT.MAXOUT) GOTO 40
            IF (N2.LT.3) GOTO 50
            CALL GRPOCL(N2, RX,   RY,   3, YL, MAXOUT, N3, QX, QY)
            IF (N3.GT.MAXOUT) GOTO 40
            IF (N3.LT.3) GOTO 50
            CALL GRPOCL(N3, QX,   QY,   4, YH, MAXOUT, N4, RX, RY)
            IF (N4.GT.MAXOUT) GOTO 40
            IF (N4.GT.0) CALL GRFA(N4,RX,RY)
            GOTO 50
 40         CALL GRWARN('PGPOLY: polygon is too complex')
 50         CONTINUE
         END IF
      END IF
C
C Set the current pen position.
C
      CALL GRMOVA(XPTS(1),YPTS(1))
      CALL PGEBUF
C
      END
