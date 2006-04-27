      SUBROUTINE sgs_SARTX (AR)
*+
*  - - - - - -
*   S A R T X
*   - - - - - -
*
*   Specify text aspect ratio.
*
*   Given:
*      AR      r      text aspect ratio (W/H)
*
*   Written to COMMON:
*      ARTX    r      text aspect ratio
*
*   Externals:
*      sgs_OTEXT, sgs_1SETTX
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL AR

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Normalise & save aspect ratio
      ARTX=MAX(ABS(AR),1E-6)

*  Translate SGS text parameters to GKS
      CALL sgs_1SETTX

      END
