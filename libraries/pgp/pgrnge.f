C*PGRNGE -- choose axis limits
C%void cpgrnge(float x1, float x2, float *xlo, float *xhi);
C+
      SUBROUTINE PGRNGE (X1, X2, XLO, XHI)
      REAL X1, X2, XLO, XHI
C
C Choose plotting limits XLO and XHI which encompass the data
C range X1 to X2.
C
C Arguments:
C  X1, X2 (input)  : the data range (X1<X2), ie, the min and max values
C                    to be plotted.
C  XLO, XHI (output) : suitable values to use as the extremes of a graph
C                    axis (XLO <= X1, XHI >= X2).
C--
C 10-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      XLO = X1 - 0.1*(X2-X1)
      XHI = X2 + 0.1*(X2-X1)
      IF (XLO.LT.0.0 .AND. X1.GE.0.0) XLO = 0.0
      IF (XHI.GT.0.0 .AND. X2.LE.0.0) XHI = 0.0
      END
