      SUBROUTINE sgs_CIRCL (XCENT,YCENT, RADIUS)
*+
*  Name:
*     CIRCL

*  Purpose:
*     Plot a circle with the current SGS pen.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     XCENT = REAL (Given)
*         Centre x (World Coordinates)
*     YCENT = REAL (Given)
*         "    y (  "        "     )
*     RADIUS = REAL (Given)
*         Radius   (  "        "     )

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
*     sgs_ARC

*-

      IMPLICIT NONE

      REAL XCENT,YCENT,RADIUS


      CALL sgs_ARC(XCENT,YCENT,RADIUS,0.0,6.2832)

      END
