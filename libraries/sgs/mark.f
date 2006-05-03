      SUBROUTINE sgs_MARK (X,Y, MTYPE)
*+
*  Name:
*     MARK

*  Purpose:
*     Draw a marker.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The marker type is set to MARKL.  The marker type ASF is assumed to
*     be set to GINDIV.

*  Arguments:
*     X = REAL (Given)
*         X coordinate of marker
*     Y = REAL (Given)
*         Y      "     "    "
*     MARKL = INTEGER (Given)
*         Marker type

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     GSMK, GPM

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
