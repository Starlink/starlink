      SUBROUTINE sgs_OPOLY
*+
*   - - - - - -
*    O P O L Y
*   - - - - - -
*
*   Output completed polyline.
*
*   Read from COMMON:
*      NPOLY       i        length of current polyline
*      XPOLY       r()      current polyline (x)
*      YPOLY       r()        "        "     (y)
*
*   Written to COMMON:
*      NPOLY       i        length of current polyline
*      XPOLY(1)    r        current polyline (x)
*      YPOLY(1)    r          "        "     (y)
*
*   Externals:
*      GPL
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INCLUDE 'sgscom'




*  Ensure polyline has been begun
      IF (NPOLY.GT.0) THEN

*     Ensure non-trivial polyline present
         IF (NPOLY.GT.1) THEN

*        Plot
            CALL GPL(NPOLY,XPOLY,YPOLY)

*        Begin new polyline
            XPOLY(1)=XPOLY(NPOLY)
            YPOLY(1)=YPOLY(NPOLY)
            NPOLY=1
         END IF
      ELSE

*  Polyline not begun or something funny - reset
         NPOLY=0
      END IF

      END
