
C*GRTRAN -- define scaling transformation
C+
      SUBROUTINE GRTRAN (IDENT,XORG,YORG,XSCALE,YSCALE)
C
C GRPCKG (internal routine): Define scaling transformation.
C
C Arguments:
C
C IDENT (input, integer): plot identifier, as returned by GROPEN.
C XORG, YORG, XSCALE, YSCALE (input, real): parameters of the scaling
C       transformation. This is defined by:
C               XABS = XORG + XWORLD * XSCALE,
C               YABS = YORG + YWORLD * YSCALE,
C       where (XABS, YABS) are the absolute device coordinates
C       corresponding to world coordinates (XWORLD, YWORLD).
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INTEGER  IDENT
      REAL     XORG, YORG, XSCALE, YSCALE
C
      CALL GRSLCT(IDENT)
      CALL GRTRN0(XORG, YORG, XSCALE, YSCALE)
C
      END
