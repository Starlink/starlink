      SUBROUTINE sgs_OPOLY
*+
*  Name:
*     OPOLY

*  Purpose:
*     Output completed polyline.

*  Language:
*     Starlink Fortran 77

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
*     GPL

*  Read From Common:
*     NPOLY       i        length of current polyline
*     XPOLY       r()      current polyline (x)
*     YPOLY       r()        "        "     (y)

*  Written To Common:
*     NPOLY       i        length of current polyline
*     XPOLY(1)    r        current polyline (x)
*     YPOLY(1)    r          "        "     (y)

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
