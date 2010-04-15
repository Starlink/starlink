      SUBROUTINE GRVCT0 (MODE,ABSXY,POINTS,X,Y)
*+
*     - - - - - - - -
*       G R V C T 0     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Draw a line or a set of dots. This is the same as GRVECT, but without
*   device selection. It can be used to draw a single line-segment, a
*   continuous series of line segments, or one or more single dots (pixels).
*
*   Given
*      MODE     i   If MODE=1, a series of line segments is drawn,
*                   starting at the current position, moving to X(1),Y(1), ...
*                   and ending at X(POINTS),Y(POINTS).
*                   If MODE=2, the first vector is blanked, so the line starts
*                   at X(1),Y(1).
*                   If MODE=3, a single dot is placed at each coordinate pair,
*                   with no connecting lines.
*      ABSXY    l   If TRUE, the coordinates are absolute device coordinates;
*                   if FALSE, they are world coordinates and the scaling
*                   transformation is applied.
*      POINTS   i   The number of coordinate pairs.
*      X, Y     r() The X and Y coordinates of the points.
*
*   D.L.Terrett  Starlink  Apr 1991 (after TJP)
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'

      INTEGER  I, MODE, POINTS
      LOGICAL  ABSXY
      REAL     X(POINTS), Y(POINTS), XCUR, YCUR

      IF (MODE.EQ.1) THEN
          CALL GRTXY0(ABSXY, X(1), Y(1), XCUR, YCUR)
          CALL GRLIN0(XCUR, YCUR)
      ELSE IF (MODE.EQ.2) THEN
          CALL GRTXY0(ABSXY, X(1), Y(1), GRXPRE(GRCIDE), GRYPRE(GRCIDE))
      END IF
      IF (MODE.EQ.1 .OR. MODE.EQ.2) THEN
          DO I=2,POINTS
            CALL GRTXY0(ABSXY, X(I), Y(I), XCUR, YCUR)
            CALL GRLIN0(XCUR, YCUR)
          END DO
      ELSE IF (MODE.EQ.3) THEN
          DO I=1,POINTS
            CALL GRTXY0(ABSXY, X(I), Y(I), XCUR, YCUR)
            CALL GRDOT0(XCUR, YCUR)
          END DO
      END IF
      RETURN
      END
