      SUBROUTINE sgs_BOX (X1,X2, Y1,Y2)
*+
*  Name:
*     BOX

*  Purpose:
*     Draw a rectangle with sides parallel to the x and y axes with the
*     current SGS pen.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     X1 = REAL (Given)
*         X coordinate of bottom left corner
*     Y1 = REAL (Given)
*         Y      "      "    "     "    "
*     X2 = REAL (Given)
*         X      "      "  top right    "
*     Y2 = REAL (Given)
*         Y      "      "   "    "      "

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
*     sgs_APOLY, sgs_BPOLY

*-

      IMPLICIT NONE

      REAL X1,X2,Y1,Y2



      CALL sgs_BPOLY(X1,Y1)
      CALL sgs_APOLY(X2,Y1)
      CALL sgs_APOLY(X2,Y2)
      CALL sgs_APOLY(X1,Y2)
      CALL sgs_APOLY(X1,Y1)

      END
