C*PGQCIR -- inquire color index range
C%void cpgqcir(int *icilo, int *icihi);
C+
      SUBROUTINE PGQCIR(ICILO, ICIHI)
      INTEGER   ICILO, ICIHI
C
C Query the color index range to be used for producing images with
C PGGRAY or PGIMAG, as set by routine PGSCIR or by device default.
C
C Arguments:
C  ICILO  (output) : the lowest color index to use for images
C  ICIHI  (output) : the highest color index to use for images
C--
C 1994-Mar-17 : new routine [AFT/TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C---
      ICILO = PGMNCI(PGID)
      ICIHI = PGMXCI(PGID)
C
      END
