      SUBROUTINE sgs_SPREC (NPR)
*+
*   - - - - - -
*    S P R E C
*   - - - - - -
*
*   Select text precision.
*
*   Given:
*      NPR       i      text precision
*
*   Read from COMMON:
*      IFONT     i      current font
*
*   Written to COMMON:
*      IPREC     i      current text precision
*
*   Externals:
*      sgs_OTEXT, GSTXFP
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER NPR

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Save precision
      IPREC=NPR

*  Set font and precision
      CALL GSTXFP(IFONT,NPR)

      END
