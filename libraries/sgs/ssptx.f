      SUBROUTINE sgs_SSPTX (SP)
*+
*   - - - - - -
*    S S P T X
*   - - - - - -
*
*   Specify text spacing.
*
*   Given:
*      SP       r     spacing factor
*
*   Written to COMMON:
*      STX      r     current character spacing
*
*   Externals:
*      sgs_OTEXT, sgs_1SETTX
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL SP

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Save spacing factor
      STX=SP

*  Translate SGS text parameters to GKS
      CALL sgs_1SETTX

      END
