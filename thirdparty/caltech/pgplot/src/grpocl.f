C*GRPOCL -- polygon clip
C+
      SUBROUTINE GRPOCL (N,PX,PY, EDGE, VAL, MAXOUT, NOUT, QX, QY)
      INTEGER N, NOUT, EDGE, MAXOUT
      REAL    PX(*), PY(*), QX(*), QY(*)
      REAL    VAL
C
C Clip a polygon against a rectangle: Sutherland-Hodgman algorithm.
C this routine must be called four times to clip against each of the
C edges of the rectangle in turn.      
C
C Arguments:
C
C N (input, integer): the number of vertices of the polygon (at least
C       3).
C PX, PY (input, real arrays, dimension at least N): world coordinates
C       of the N vertices of the input polygon.
C EDGE (input, integer):
C     1: clip against left edge,   X > XMIN=VAL
C     2: clip against right edge,  X < XMAX=VAL
C     3: clip against bottom edge, Y > YMIN=VAL
C     4: clip against top edge,    Y < YMIN=VAL
C VAL  (input, real): coordinate value of current edge.
C MAXOUT (input, integer): maximum number of vertices allowed in
C     output polygon (dimension of QX, QY).
C NOUT (output, integer): the number of vertices in the clipped polygon.
C QX, QY (output, real arrays, dimension at least MAXOUT): world
C       coordinates of the NOUT vertices of the output polygon.
C--
C 19-Sep-1994 - [TJP].
C 27-Feb-1996 - fix bug: overflow if coordinates are large [TJP].
C 11-Jul-1996 - fix bug: left and bottom edges disappeared when precisely
C               on edge [Remko Scharroo]
C-----------------------------------------------------------------------
      INTEGER I
      REAL FX, FY, SX, SY
C
      NOUT = 0
      DO 100 I=1,N
         IF (I.EQ.1) THEN
C           -- save first point
            FX = PX(I)
            FY = PY(I)
         ELSE IF ((EDGE.EQ.1 .OR.EDGE.EQ.2) .AND.
     :            (SIGN(1.0,PX(I)-VAL).NE.SIGN(1.0,SX-VAL))) THEN
C           -- SP intersects this edge: output vertex at intersection
            NOUT = NOUT+1
            IF (NOUT.LE.MAXOUT) THEN
               QX(NOUT) = VAL
               QY(NOUT) = SY + (PY(I)-SY)*((VAL-SX)/(PX(I)-SX))
            END IF
         ELSE IF ((EDGE.EQ.3 .OR.EDGE.EQ.4) .AND.
     :            (SIGN(1.0,PY(I)-VAL).NE.SIGN(1.0,SY-VAL))) THEN
C           -- SP intersects this edge: output vertex at intersection
            NOUT = NOUT+1
            IF (NOUT.LE.MAXOUT) THEN
               QX(NOUT) = SX + (PX(I)-SX)*((VAL-SY)/(PY(I)-SY))
               QY(NOUT) = VAL
            END IF
         END IF
         SX = PX(I)
         SY = PY(I)
         IF ((EDGE.EQ.1.AND.SX.GE.VAL) .OR.
     :       (EDGE.EQ.2.AND.SX.LE.VAL) .OR.
     :       (EDGE.EQ.3.AND.SY.GE.VAL) .OR.
     :       (EDGE.EQ.4.AND.SY.LE.VAL)) THEN
C           -- output visible vertex S
            NOUT = NOUT + 1
            IF (NOUT.LE.MAXOUT) THEN
                QX(NOUT) = SX
                QY(NOUT) = SY
            END IF
         END IF
 100  CONTINUE
C      -- Does SF intersect edge?
      IF ((EDGE.EQ.1 .OR. EDGE.EQ.2) .AND.
     :    (SIGN(1.0,SX-VAL).NE.SIGN(1.0,FX-VAL))) THEN
         NOUT = NOUT+1
         IF (NOUT.LE.MAXOUT) THEN
            QX(NOUT) = VAL
            QY(NOUT) = SY + (FY-SY)*((VAL-SX)/(FX-SX))
         END IF
      ELSE IF ((EDGE.EQ.3 .OR. EDGE.EQ.4) .AND.
     :         (SIGN(1.0,SY-VAL).NE.SIGN(1.0,FY-VAL))) THEN
         NOUT = NOUT+1
         IF (NOUT.LE.MAXOUT) THEN
            QY(NOUT) = VAL
            QX(NOUT) = SX + (FX-SX)*((VAL-SY)/(FY-SY))
         END IF
      END IF
C
      END
