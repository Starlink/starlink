      SUBROUTINE sgs_IPLXY (X,Y)
*+
*   - - - - - -
*    I P L X Y
*   - - - - - -
*
*   Inquire end of current polyline.
*
*   X,Y are unchanged if there is no current polyline.
*
*   Returned:
*        X      r      x coordinate of end of current polyline
*        Y      r      y     "      "   "  "     "       "
*
*   Read from COMMON:
*      NPOLY    i      length of current polyline
*      XPOLY    r()    current polyline (X)
*      YPOLY    r()       "       "     (Y)
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL X,Y

      INCLUDE 'sgscom'




      IF (NPOLY.GT.0) THEN
         X = XPOLY(NPOLY)
         Y = YPOLY(NPOLY)
      END IF

      END
