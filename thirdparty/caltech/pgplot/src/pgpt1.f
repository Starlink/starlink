C*PGPT1 -- draw one graph marker
C%void cpgpt1(float xpt, float ypt, int symbol);
C+
      SUBROUTINE PGPT1 (XPT, YPT, SYMBOL)
      REAL XPT, YPT
      INTEGER SYMBOL
C
C Primitive routine to draw a single Graph Marker at a specified point.
C The marker is drawn using the current values of attributes
C color-index, line-width, and character-height (character-font applies
C if the symbol number is >31).  If the point to be marked lies outside
C the window, no marker is drawn.  The "pen position" is changed to
C (XPT,YPT) in world coordinates.
C
C To draw several markers with coordinates specified by X and Y
C arrays, use routine PGPT.
C
C Arguments:
C  XPT    (input)  : world x-coordinate of the point.
C  YPT    (input)  : world y-coordinate of the point.
C  SYMBOL (input)  : code number of the symbol to be drawn:
C                    -1, -2  : a single dot (diameter = current
C                              line width).
C                    -3..-31 : a regular polygon with ABS(SYMBOL)
C                              edges (style set by current fill style).
C                    0..31   : standard marker symbols.
C                    32..127 : ASCII characters (in current font).
C                              e.g. to use letter F as a marker, let
C                              SYMBOL = ICHAR('F'). 
C                    > 127  :  a Hershey symbol number.
C--
C  4-Feb-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
      REAL XPTS(1), YPTS(1)
C
      IF (PGNOTO('PGPT1')) RETURN
      XPTS(1) = XPT
      YPTS(1) = YPT
      CALL PGPT(1, XPTS, YPTS, SYMBOL)
      END
