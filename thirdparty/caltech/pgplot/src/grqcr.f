C*GRQCR -- inquire color representation
C+
      SUBROUTINE GRQCR (CI, CR, CG, CB)
      INTEGER  CI
      REAL     CR, CG, CB
C
C Return the color representation (red, green, blue intensities) 
C currently associated with the specified color index. This may be
C different from that requested on some devices.
C
C Arguments:
C
C CI (integer, input): color index.
C CR, CG, CB (real, output): red, green, and blue intensities,
C       in range 0.0 to 1.0.
C--
C  7-Sep-1994 - rewrite [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER   NBUF, LCHR, K
      REAL      RBUF(6)
      CHARACTER CHR
C
      CR = 1.0
      CG = 1.0
      CB = 1.0
      K  = CI
      IF (GRCIDE.LT.1) THEN
C         -- no device open: return white
          CALL GRWARN('GRQCR: no plot device is open.')
      ELSE IF (GRGCAP(GRCIDE)(9:9).NE.'Y') THEN
C         -- devices that don't allow query color representation:
C            return black for ci 0, white for all others
          IF (K.EQ.0) THEN
             CR = 0.0
             CG = 0.0
             CB = 0.0
          END IF
      ELSE
C         -- query device driver; treat invalid ci as 1
          IF (K.LT.GRMNCI(GRCIDE) .OR. CI.GT.GRMXCI(GRCIDE)) THEN
             CALL GRWARN('GRQCR: invalid color index.')
             K = 1
          END IF
          RBUF(1) = K
          NBUF = 1
          LCHR = 0
          CALL GREXEC(GRGTYP,29,RBUF,NBUF,CHR,LCHR)
          IF (NBUF.LT.4) THEN
             CALL GRWARN('GRSCR: device driver error')
          ELSE
              CR = RBUF(2)
              CG = RBUF(3)
              CB = RBUF(4)
          END IF
      END IF
C
      END
