
C*GRVECT -- draw line segments or dots
C+
      SUBROUTINE GRVECT (IDENT,MODE,ABSXY,POINTS,X,Y)
C
C GRPCKG: Draw a line or a set of dots. This routine can be used to
C draw a single line-segment, a continuous series of line segments, or
C one or more single dots (pixels).
C
C Arguments:
C
C IDENT (input, integer): the plot identifier, as returned by GROPEN.
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
      INTEGER  IDENT, MODE, POINTS
      LOGICAL  ABSXY
      REAL     X(POINTS), Y(POINTS)
C
      CALL GRSLCT(IDENT)
      IF (MODE.LE.0 .OR. MODE.GT.3) THEN
          CALL GRWARN('GRVECT - invalid MODE parameter.')
      ELSE IF (POINTS.GT.0) THEN
          CALL GRVCT0(MODE, ABSXY, POINTS, X, Y)
      END IF
C
      END
