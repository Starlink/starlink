      SUBROUTINE sgs_LINE (X1,Y1, X2,Y2)
*+
*   - - - - -
*    L I N E
*   - - - - -
*
*   Begin a new polyline with a single line.
*
*   Given:
*        X1      r     starting x coordinate for polyline
*        Y2      r        "     y     "       "     "
*        X2      r     ending   x     "       "     "
*        Y2      r        "     y     "       "     "
*
*   Externals:
*      sgs_BPOLY, sgs_APOLY
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL X1,Y1,X2,Y2



*  Begin the new polyline
      CALL sgs_BPOLY(X1,Y1)

*  Append the line
      CALL sgs_APOLY(X2,Y2)

      END
