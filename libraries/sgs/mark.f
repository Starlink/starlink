      SUBROUTINE sgs_MARK (X,Y, MTYPE)
*+
*   - - - - -
*    M A R K
*   - - - - -
*
*   Draw a marker.
*
*   The marker type is set to MARKL.  The marker type ASF is assumed to
*   be set to GINDIV.
*
*   Given:
*      X       r     x coordinate of marker
*      Y       r     y      "     "    "
*      MARKL   i     marker type
*
*   Externals:
*      GSMK, GPM
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL X,Y
      INTEGER MTYPE

      REAL XP(1),YP(1)



*  Set the marker type
      CALL GSMK(MTYPE)

*  Draw the marker
      XP(1)=X
      YP(1)=Y
      CALL GPM(1,XP,YP)

      END
