      SUBROUTINE sgs_BPOLY (X,Y)
*+
*
*   - - - - - -
*    B P O L Y
*   - - - - - -
*
*   Begin a new polyline.
*
*   Given:
*      X         r      x coordinate of first point
*      Y         r      y      "      "   "     "
*
*   Read from COMMON:
*      NPOLY     i      length of current polyline
*
*   Written to COMMON:
*      XPOLY     r()    current polyline buffer (X)
*      YPOLY     r()    current polyline buffer (Y)
*      NPOLY     i      length of current polyline
*
*   Externals:
*      sgs_OPOLY
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL X,Y

      INCLUDE 'sgscom'




*  Flush any existing polyline
      IF (NPOLY.GT.1) CALL sgs_OPOLY

*  Copy starting X,Y
      XPOLY(1)=X
      YPOLY(1)=Y

*  Initialise length
      NPOLY=1

      END
