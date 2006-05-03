      SUBROUTINE sgs_LINE (X1,Y1, X2,Y2)
*+
*  Name:
*     LINE

*  Purpose:
*     Begin a new polyline with a single line.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     X1 = REAL (Given)
*         Starting x coordinate for polyline
*     Y2 = REAL (Given)
*         "     y     "       "     "
*     X2 = REAL (Given)
*         Ending   x     "       "     "
*     Y2 = REAL (Given)
*         "     y     "       "     "

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
*     sgs_BPOLY, sgs_APOLY

*-

      IMPLICIT NONE

      REAL X1,Y1,X2,Y2



*  Begin the new polyline
      CALL sgs_BPOLY(X1,Y1)

*  Append the line
      CALL sgs_APOLY(X2,Y2)

      END
