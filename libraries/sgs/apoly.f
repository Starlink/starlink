      SUBROUTINE sgs_APOLY (X,Y)
*+
*  - - - - - -
*   A P O L Y
*  - - - - - -
*
*  Append new point to current polyline.
*
*  If the polyline buffer is full, call OPOLY to flush the polyline to
*  GKS and start a new polyline beginning at the last point of the old
*  one.  Copy the new point into the buffer and increment the number of
*  points.
*
*  Given:
*     X      r     x coordinate of new point
*     Y      r     y     "      "   "    "
*
*  Read from COMMON:
*     NPOLY  i     Length of current polyline
*
*  Written to COMMON:
*     XPOLY  r()   Polyline buffer (X)
*     XPOLY  r()   Polyline buffer (Y)
*     NPOLY  i     Length of current polyline
*
*  Constants (from SGSCOM):
*     LPOLY  i     Maximum number of points in polyline
*
*  Externals:
*     sgs_OPOLY
*
*  P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      REAL X,Y

      INCLUDE 'sgscom'



*  Accept append request only if polyline has been begun
      IF (NPOLY.GT.0) THEN

*     If buffer full, flush
         IF (NPOLY.GE.LPOLY) CALL sgs_OPOLY

*     Append new line
         NPOLY=NPOLY+1
         XPOLY(NPOLY)=X
         YPOLY(NPOLY)=Y
      END IF

      END
