      SUBROUTINE sgs_1SETTX
*+
*   - - - - - -
*    S E T T X     (Internal routine)
*   - - - - - -
*
*   Set text size spacing & justification from SGS parameters.
*
*   Read from COMMON:
*      HTX       r      text height
*      XUPTX     r      character up vector (x)
*      YUPTX     r         "      "    "    (y)
*      STX       r      character spacing
*      CTXJ      c*2    text justification
*      ARTX      r      character aspect ratio
*
*   Constants from GKS_PAR:
*      GAHALF    i      vertical text alignment - Halfline
*      GACAP     i         "      "       "     - Capline
*      GABASE    i         "      "       "     - Baseline
*      GACENT    i      horizontal text alignment - Centre
*      GALEFT    i          "        "      "     - Left
*      GARITE    i          "        "      "     - Right
*
*   Externals:
*      GSCHH, GSCHUP, GSCHSP, GSTXAL, GSCHXP
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'


      INTEGER TXALV,TXALH

      CHARACTER*5 RNAME
      PARAMETER (RNAME='SETTX')

*  Nominal aspect ratio of "normal" fonts
      REAL DEFASP
      PARAMETER (DEFASP=2.0/3.0)



*  Set height
      CALL GSCHH(HTX)

*  Set up-vector
      CALL GSCHUP(XUPTX,YUPTX)

*  Set spacing
      CALL GSCHSP(STX*DEFASP)

*  Convert justifications to GKS equivalents
      IF (CTXJ(1:1).EQ.'C') TXALV = GAHALF
      IF (CTXJ(1:1).EQ.'T') TXALV = GACAP
      IF (CTXJ(1:1).EQ.'B') TXALV = GABASE

      IF (CTXJ(2:2).EQ.'C') TXALH = GACENT
      IF (CTXJ(2:2).EQ.'L') TXALH = GALEFT
      IF (CTXJ(2:2).EQ.'R') TXALH = GARITE

      CALL GSTXAL(TXALH,TXALV)

*  Set expansion factor
      CALL GSCHXP(ARTX/DEFASP)

      END
