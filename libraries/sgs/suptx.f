      SUBROUTINE sgs_SUPTX (XU,YU)
*+
*   - - - - - -
*    S U P T X
*   - - - - - -
*
*   Specify text orientation vector.
*
*   Given:
*      XU        r      x 'up' vector (magnitude immaterial)
*      YU        r      y  "     "        "          "
*
*   Written to COMMON:
*      XUPTX     r      current up vector (x)
*      YUPTX     r        "     "    "    (y)
*
*   Externals:
*      sgs_OTEXT, sgs_1SETTX
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
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
