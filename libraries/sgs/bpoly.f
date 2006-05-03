      SUBROUTINE sgs_BPOLY (X,Y)
*+
*  Name:
*     BPOLY

*  Purpose:
*     Begin a new polyline.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     X = REAL (Given)
*         X coordinate of first point
*     Y = REAL (Given)
*         Y      "      "   "     "

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
*     sgs_OPOLY

*  Read From Common:
*     NPOLY     i      length of current polyline

*  Written To Common:
*     XPOLY     r()    current polyline buffer (X)
*     YPOLY     r()    current polyline buffer (Y)
*     NPOLY     i      length of current polyline

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
