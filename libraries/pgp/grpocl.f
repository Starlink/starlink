      SUBROUTINE GRPOCL (N,PX,PY, EDGE, VAL, MAXOUT, NOUT, QX, QY)
*+
*
*     - - - - - - - -
*       G R P O C L    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Clip a polygon against a rectangle: Sutherland-Hodgman algorithm.
*   this routine must be called four times to clip against each of the
*   edges of the rectangle in turn.
*
*   Given
*      N        i       Number of vertices
*      PX       r()     X coordinates of verticies
*      PY       r()     Y     "       "      "
*      XSIZE    r       X size in absolute coordinates
*      YSIZE    r       Y   "  "      "        "
*      EDGE     i       1: clip against left edge,   X > XMIN=VAL
*                       2: clip against right edge,  X < XMAX=VAL
*                       3: clip against bottom edge, Y > YMIN=VAL
*                       4: clip against top edge,    Y < YMIN=VAL
*      VAL      r       coordinate value of current edge.
*      MAXOUT   i       maximum number of vertices allowed in output
*                       polygon (dimension of QX, QY).
*
*   Returned
*     PX        r(>=N)  X world coordinates of the N vertices of the
*                       input polygon.
*     PY        r(>=N)  Y ditto
*     NOUT      i       the number of vertices in the clipped polygon.
*     QX        r(>=MAXOUT)  X world coordinates of the NOUT vertices of i
*                            the output polygon.
*     QY        r(>=MAXOUT) Y ditto
*
*   D.L.Terrett  Starlink  Feb 1995 (After Tim Pearson)
*-
      IMPLICIT NONE
      INTEGER N, NOUT, EDGE, MAXOUT
      REAL    PX(N), PY(N), QX(*), QY(*)
      REAL    VAL

      INTEGER I
      REAL FX, FY, SX, SY

      NOUT = 0
      DO 100 I=1,N
         IF (I.EQ.1) THEN
*           -- save first point
            FX = PX(I)
            FY = PY(I)
         ELSE IF ((EDGE.EQ.1 .OR.EDGE.EQ.2) .AND.
     :            (PX(I)-VAL)*(SX-VAL).LT.0.0) THEN
*           -- SP intersects this edge: output vertex at intersection
            NOUT = NOUT+1
            IF (NOUT.LE.MAXOUT) THEN
               QX(NOUT) = VAL
               QY(NOUT) = SY + (PY(I)-SY)*(VAL-SX)/(PX(I)-SX)
            END IF
         ELSE IF ((EDGE.EQ.3 .OR.EDGE.EQ.4) .AND.
     :            (PY(I)-VAL)*(SY-VAL).LT.0.0) THEN
*           -- SP intersects this edge: output vertex at intersection
            NOUT = NOUT+1
            IF (NOUT.LE.MAXOUT) THEN
               QX(NOUT) = SX + (PX(I)-SX)*(VAL-SY)/(PY(I)-SY)
               QY(NOUT) = VAL
            END IF
         END IF
         SX = PX(I)
         SY = PY(I)
         IF ((EDGE.EQ.1.AND.SX.GT.VAL) .OR.
     :       (EDGE.EQ.2.AND.SX.LE.VAL) .OR.
     :       (EDGE.EQ.3.AND.SY.GT.VAL) .OR.
     :       (EDGE.EQ.4.AND.SY.LE.VAL)) THEN
*           -- output visible vertex S
            NOUT = NOUT + 1
            IF (NOUT.LE.MAXOUT) THEN
                QX(NOUT) = SX
                QY(NOUT) = SY
            END IF
         END IF
 100  CONTINUE
*      -- Does SF intersect edge?
      IF ((EDGE.EQ.1 .OR. EDGE.EQ.2) .AND.
     :     (SX-VAL)*(FX-VAL).LT.0.0) THEN
         NOUT = NOUT+1
         IF (NOUT.LE.MAXOUT) THEN
            QX(NOUT) = VAL
            QY(NOUT) = SY + (FY-SY)*(VAL-SX)/(FX-SX)
         END IF
      ELSE IF ((EDGE.EQ.3 .OR. EDGE.EQ.4) .AND.
     :     (SY-VAL)*(FY-VAL).LT.0.0) THEN
         NOUT = NOUT+1
         IF (NOUT.LE.MAXOUT) THEN
            QY(NOUT) = VAL
            QX(NOUT) = SX + (FX-SX)*(VAL-SY)/(FY-SY)
         END IF
      END IF
*
      END
