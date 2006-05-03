      SUBROUTINE sgs_SUPTX (XU,YU)
*+
*  Name:
*     SUPTX

*  Purpose:
*     Specify text orientation vector.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     XU = REAL (Given)
*         X 'up' vector (magnitude immaterial)
*     YU = REAL (Given)
*         Y  "     "        "          "

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
*     sgs_OTEXT, sgs_1SETTX

*  Written To Common:
*     XUPTX     r      current up vector (x)
*     YUPTX     r        "     "    "    (y)

*-

      IMPLICIT NONE

      REAL XU,YU

      REAL R,RLMIN
      PARAMETER (RLMIN = 1.0E-35)

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Normalise & save vector
      R=SQRT(XU*XU+YU*YU)
      IF (R.GT.RLMIN) THEN
         XUPTX=XU/R
         YUPTX=YU/R
      ELSE
         XUPTX=0.0
         YUPTX=1.0
      END IF

*  Translate SGS text parameters to GKS
      CALL sgs_1SETTX

      END
