C*GRCLIP -- clip a point against clipping rectangle
C+
      SUBROUTINE GRCLIP (X,Y,XMIN,XMAX,YMIN,YMAX,C)
      REAL X,Y
      REAL XMIN,XMAX,YMIN,YMAX
      INTEGER C
C
C GRPCKG (internal routine): support routine for the clipping algorithm;
C called from GRLIN0 only. C is a 4 bit code indicating the relationship
C between point (X,Y) and the window boundaries; 0 implies the point is
C within the window.
C
C Arguments:
C--
C (11-Feb-1983)
C Revised 20-Jun-1985 (TJP); use floating arithmetic
C Revised 12-Jun-1992 (TJP); clip exactly on the boundary
C-----------------------------------------------------------------------
C
      C = 0
      IF (X.LT.XMIN) THEN
          C = 1
      ELSE IF (X.GT.XMAX) THEN
          C = 2
      END IF
      IF (Y.LT.YMIN) THEN
          C = C+4
      ELSE IF (Y.GT.YMAX) THEN
          C = C+8
      END IF
      END
