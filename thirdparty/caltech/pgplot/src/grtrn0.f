
C*GRTRN0 -- define scaling transformation
C+
      SUBROUTINE GRTRN0 (XORG,YORG,XSCALE,YSCALE)
C
C GRPCKG (internal routine): Define scaling transformation for current
C device (equivalent to GRTRAN without device selection).
C
C Arguments:
C
C XORG, YORG, XSCALE, YSCALE (input, real): parameters of the scaling
C       transformation. This is defined by:
C               XABS = XORG + XWORLD * XSCALE,
C               YABS = YORG + YWORLD * YSCALE,
C       where (XABS, YABS) are the absolute device coordinates
C       corresponding to world coordinates (XWORLD, YWORLD).
C--
C  1-Feb-83:
C 11-Feb-92: Add driver support (TJP).
C  1-Sep-94: Suppress driver call (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     XORG, YORG, XSCALE, YSCALE
      REAL           RBUF(6)
      INTEGER        NBUF,LCHR
      CHARACTER*16   CHR
C
      GRXORG(GRCIDE) = XORG
      GRXSCL(GRCIDE) = XSCALE
      GRYORG(GRCIDE) = YORG
      GRYSCL(GRCIDE) = YSCALE
C
C Pass info to device driver?
C
      IF (GRGCAP(GRCIDE)(2:2).EQ.'X') THEN
          RBUF(1) = XORG
          RBUF(2) = XSCALE
          RBUF(3) = YORG
          RBUF(4) = YSCALE
          NBUF = 4
          LCHR = 0
          CALL GREXEC(GRGTYP,27,RBUF,NBUF,CHR,LCHR)
      END IF
C
      END
