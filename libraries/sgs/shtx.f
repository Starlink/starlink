      SUBROUTINE sgs_SHTX (H)
*+
*   - - - - -
*    S H T X
*   - - - - -
*
*   Specify text height.
*
*   Given:
*      H         r      text height (sign ignored)
*
*   Written to COMMON:
*      HTX       r      current txt height
*
*   Externals:
*      sgs_OTEXT, sgs_1SETTX
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL H

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Save height
      HTX=ABS(H)

*  Translate SGS text parameters to GKS
      CALL sgs_1SETTX

      END
