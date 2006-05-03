      SUBROUTINE sgs_APOLY (X,Y)
*+
*  Name:
*     APOLY

*  Purpose:
*     Append new point to current polyline.

*  Language:
*     Starlink Fortran 77

*  Description:
*     If the polyline buffer is full, call OPOLY to flush the polyline to
*     GKS and start a new polyline beginning at the last point of the old
*     one.  Copy the new point into the buffer and increment the number of
*     points.

*  Arguments:
*     X = REAL (Given)
*         X coordinate of new point
*     Y = REAL (Given)
*         Y     "      "   "    "

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     sgs_OPOLY

*  Read From Common:
*     NPOLY  i     Length of current polyline

*  Written To Common:
*     XPOLY  r()   Polyline buffer (X)
*     XPOLY  r()   Polyline buffer (Y)
*     NPOLY  i     Length of current polyline
*
*  Constants (from SGSCOM):
*     LPOLY  i     Maximum number of points in polyline

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
