      SUBROUTINE sgs_SFONT (NF)
*+
*   - - - - - -
*    S F O N T
*   - - - - - -
*
*   Select text font.
*
*   Given:
*      NF       i      font number
*
*   Read from COMMON:
*      IPREC    i      text precision
*
*   Written to COMMON:
*      IFONT    i      Current font number
*
*   Externals:
*      sgs_OTEXT, GSTXFP
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER NF

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Save font number
      IFONT=NF

*  Set font and precision
      CALL GSTXFP(NF,IPREC)

      END
