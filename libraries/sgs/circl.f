      SUBROUTINE sgs_CIRCL (XCENT,YCENT, RADIUS)
*+
*   - - - - - -
*    C I R C L
*   - - - - - -
*
*   Plot a circle with the current SGS pen.
*
*   Given:
*      XCENT     r     centre x (World Coordinates)
*      YCENT     r       "    y (  "        "     )
*      RADIUS    r     radius   (  "        "     )
*
*   Externals:
*      sgs_ARC
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      REAL XCENT,YCENT,RADIUS


      CALL sgs_ARC(XCENT,YCENT,RADIUS,0.0,6.2832)

      END
