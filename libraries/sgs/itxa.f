      SUBROUTINE sgs_ITXA (IF, IPR, HT, AR, XU,YU, SP, TXJ)
*+
*   - - - - -
*    I T X A
*   - - - - -
*
*   Inquire text attributes.
*
*   Returned:
*      IF          i     text font number
*      IPR         i     text precision
*      HT          r     text height
*      AR          r     text aspect ratio (H/W)
*      XU          r     text orientation direction cosine (x)
*      YU          r       "      "           "       "    (y)
*      SP          r     text spacing
*      TXJ         c     text justification code
*
*   Read from COMMON:
*      IFONT       i     text font number
*      IPREC       i     text precision
*      HTX         r     text height
*      ARTX        r     text aspect ratio (H/W)
*      XUPTX       r     text orientation direction cosine (x)
*      YUPTX       r       "       "          "        "   (y)
*      STX         r     text spacing
*      CTXJ        c     text justification code
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER IF,IPR
      REAL HT,AR,XU,YU,SP
      CHARACTER*2 TXJ

      INCLUDE 'sgscom'




      IF = IFONT
      IPR = IPREC
      HT = HTX
      AR = ARTX
      XU = XUPTX
      YU = YUPTX
      SP = STX
      TXJ = CTXJ

      END
