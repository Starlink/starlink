      REAL FUNCTION PGHIS1 (X, NELMX, CENTER, IXV)
      LOGICAL CENTER
      INTEGER NELMX, IXV
      REAL X(NELMX)
C
C PGPLOT Internal routine used by PGHI2D.  Calculates the X-value for
C the left hand edge of a given element of the array being plotted.
C
C Arguments -
C
C X (input, real array): abscissae of bins
C NELMX (input, integer): number of bins
C CENTER (Input, logical): if .true., X values denote the center of
C       the bin; if .false., the X values denote the lower edge (in X)
C       of the bin.
C IXV (input, integer): the bin number in question.  Note IXV may be
C       outside the range 1..NELMX, in which case an interpolated
C       value is returned.
C
C 21-Feb-1984 - Keith Shortridge.
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C-----------------------------------------------------------------------
      REAL XN
      INTRINSIC REAL
C
      IF (CENTER) THEN
          IF ((IXV.GT.1).AND.(IXV.LE.NELMX)) THEN
            XN = ( X(IXV-1) + X(IXV) ) * .5
          ELSE IF (IXV.LE.1) THEN
            XN = X(1) - .5 * (X(2) - X(1)) * REAL(3 - 2 * IXV)
          ELSE IF (IXV.GT.NELMX) THEN
            XN = X(NELMX) +.5*(X(NELMX)-X(NELMX-1))*
     1           REAL((IXV-NELMX)*2-1)
          END IF
      ELSE
          IF ((IXV.GE.1).AND.(IXV.LE.NELMX)) THEN
            XN = X(IXV)
          ELSE IF (IXV.LT.1) THEN
            XN = X(1) - ( X(2) - X(1) ) * REAL( 1 - IXV )
          ELSE IF (IXV.GT.NELMX) THEN
            XN = X(NELMX) + ( X(NELMX) - X(NELMX-1)) *
     1           REAL(IXV - NELMX)
          END IF
      END IF
C
      PGHIS1 = XN
      END
