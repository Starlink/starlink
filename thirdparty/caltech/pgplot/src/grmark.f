
C*GRMARK -- mark points with specified symbol
C+
      SUBROUTINE GRMARK (IDENT,CENTER,SYMBOL,ABSXY,POINTS,X,Y)
C
C GRPCKG: mark a sequence of points with a specified symbol. The
C plot is windowed in the current subarea.
C
C Arguments:
C
C IDENT (integer, input): plot identifier from GROPEN.
C CENTER (input, logical): if .TRUE. the symbol is centered on the point,
C      otherwise the bottom left corner is placed at the point.
C SYMBOL (byte or integer, input): code number of symbol in range 0-127
C      (ASCII character or special symbol); if SYMBOL is outside this
C      range, nothing is plotted.
C ABSXY (logical, input): if .TRUE. (X,Y) are absolute (device)
C      coordinates; otherwise they are world coordinates and the
C      scaling transformation is applied.
C POINTS (integer, input): the number of points; if POINTS is less than
C      or equal to 0, nothing is plotted.
C X,Y (real arrays, dimension at least POINTS, input): the coordinate
C      pairs; if POINTS=1, these may be scalars instead of arrays.
C
C (9-Mar-1983)
C-----------------------------------------------------------------------
      INTEGER  SYMBOL
      CHARACTER*1 MARK
      INTEGER  I, IDENT, POINTS
      LOGICAL  ABSXY, CENTER
      REAL     X(*), Y(*)
C-----------------------------------------------------------------------
      IF (POINTS.LE.0 .OR. SYMBOL.LT.0 .OR. SYMBOL.GT.127) RETURN
      CALL GRSLCT(IDENT)
      MARK = CHAR(SYMBOL)
      DO 10 I=1,POINTS
          CALL GRCHR0(.TRUE., CENTER, 0.0, ABSXY, X(I), Y(I), MARK)
   10 CONTINUE
C-----------------------------------------------------------------------
      END
