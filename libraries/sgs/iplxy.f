      SUBROUTINE sgs_IPLXY (X,Y)
*+
*  Name:
*     IPLXY

*  Purpose:
*     Inquire end of current polyline.

*  Language:
*     Starlink Fortran 77

*  Description:
*     X,Y are unchanged if there is no current polyline.

*  Arguments:
*     X = REAL (Returned)
*         X coordinate of end of current polyline
*     Y = REAL (Returned)
*         Y     "      "   "  "     "       "

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

*  Read From Common:
*     NPOLY    i      length of current polyline
*     XPOLY    r()    current polyline (X)
*     YPOLY    r()       "       "     (Y)

*-

      IMPLICIT NONE

      REAL X,Y

      INCLUDE 'sgscom'




      IF (NPOLY.GT.0) THEN
         X = XPOLY(NPOLY)
         Y = YPOLY(NPOLY)
      END IF

      END
