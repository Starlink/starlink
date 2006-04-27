      SUBROUTINE sgs_MARKL (MTYPE)
*+
*   - - - - - -
*    M A R K L
*   - - - - - -
*
*   Draw marker at current end of polyline.
*
*   The marker type is set to MARKL. The marker type ASF is assumed to
*   be set to GINDIV.
*
*   If no polyline has yet been begun no marker is drawn.
*
*   Given:
*      MARKL      i      marker type
*
*   Read from COMMON:
*      NPOLY      i      length of current polyline
*      XPOLY      i()    current polyline (X)
*      YPOLY      i()       "       "     (Y)
*
*   Externals:
*      sgs_OPOLY, GSMK
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER MTYPE

      INCLUDE 'sgscom'



*  Make sure there's a polyline
      IF (NPOLY.GT.0) THEN

*     Flush it
         CALL sgs_OPOLY

*     Set the marker type
         CALL GSMK(MTYPE)

*     Draw the marker
         CALL GPM(1,XPOLY,YPOLY)
      END IF

      END
