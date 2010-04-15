      SUBROUTINE GRRECT (X1,Y1,X2,Y2)
*++
*   GRRECT   Fill a rectangle
*
*   Description:
*      Fills a rectangle by calling GRFA
*
*   Given:
*      X1      r     X coordinate of corner 1
*      Y1      r     Y     "      "    "    1
*      X2      r     X     "      "    "    2
*      Y2      r     Y     "      "    "    2
*
*   D L Terrett  20-MAY-1988
*++
      IMPLICIT NONE
      REAL X1,X2,Y1,Y2

      REAL X(4),Y(4)

      X(1) = X1
      X(2) = X2
      X(3) = X2
      X(4) = X1
      Y(1) = Y1
      Y(2) = Y1
      Y(3) = Y2
      Y(4) = Y2

      CALL GRFA(4,X,Y)

      END
