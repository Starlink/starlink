
C*GRVCT0 -- draw line segments or dots
C+
      SUBROUTINE GRVCT0 (MODE,ABSXY,POINTS,X,Y)
C
C GRPCKG (internal routine): Draw a line or a set of dots. This
C is the same as GRVECT, but without device selection. It can be used to
C draw a single line-segment, a continuous series of line segments, or
C one or more single dots (pixels).
C
C Arguments:
C
C MODE (input, integer): if MODE=1, a series of line segments is drawn,
C       starting at the current position, moving to X(1),Y(1), ... and
C       ending at X(POINTS),Y(POINTS).
C       If MODE=2, the first vector is blanked, so the line starts at
C       X(1),Y(1).
C       If MODE=3, a single dot is placed at each coordinate pair, with
C       no connecting lines.
C ABSXY (input, logical): if TRUE, the coordinates are absolute device
C       coordinates; if FALSE, they are world coordinates and the
C       scaling transformation is applied.
C POINTS (input, integer): the number of coordinate pairs.
C X, Y (input, real arrays, dimensioned POINTS or greater): the
C       X and Y coordinates of the points.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I, MODE, POINTS
      LOGICAL  ABSXY
      REAL     X(POINTS), Y(POINTS), XCUR, YCUR
C
      IF (MODE.EQ.1) THEN
          CALL GRTXY0(ABSXY, X(1), Y(1), XCUR, YCUR)
          CALL GRLIN0(XCUR, YCUR)
      ELSE IF (MODE.EQ.2) THEN
          CALL GRTXY0(ABSXY, X(1), Y(1), GRXPRE(GRCIDE), GRYPRE(GRCIDE))
      END IF
      IF (MODE.EQ.1 .OR. MODE.EQ.2) THEN
          DO 10 I=2,POINTS
              CALL GRTXY0(ABSXY, X(I), Y(I), XCUR, YCUR)
              CALL GRLIN0(XCUR, YCUR)
   10     CONTINUE
      ELSE IF (MODE.EQ.3) THEN
          DO 20 I=1,POINTS
              CALL GRTXY0(ABSXY, X(I), Y(I), XCUR, YCUR)
              CALL GRDOT0(XCUR, YCUR)
   20     CONTINUE
      END IF
C
      END
