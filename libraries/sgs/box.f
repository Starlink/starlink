      SUBROUTINE sgs_BOX (X1,X2, Y1,Y2)
*+
*   - - - -
*    B O X
*   - - - -
*
*   Draw a rectangle with sides parallel to the x and y axes with the
*   current SGS pen.
*
*   Given:
*      X1    r    x coordinate of bottom left corner
*      Y1    r    y      "      "    "     "    "
*      X2    r    x      "      "  top right    "
*      Y2    r    y      "      "   "    "      "
*
*   Externals:
*      sgs_APOLY, sgs_BPOLY
*
*  P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      REAL X1,X2,Y1,Y2



      CALL sgs_BPOLY(X1,Y1)
      CALL sgs_APOLY(X2,Y1)
      CALL sgs_APOLY(X2,Y2)
      CALL sgs_APOLY(X1,Y2)
      CALL sgs_APOLY(X1,Y1)

      END
