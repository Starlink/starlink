C*PGSCIR -- set color index range
C%void cpgscir(int icilo, int icihi);
C+
      SUBROUTINE PGSCIR(ICILO, ICIHI)
      INTEGER   ICILO, ICIHI
C
C Set the color index range to be used for producing images with
C PGGRAY or PGIMAG. If the range is not all within the range supported
C by the device, a smaller range will be used. The number of
C different colors available for images is ICIHI-ICILO+1.
C
C Arguments:
C  ICILO  (input)  : the lowest color index to use for images
C  ICIHI  (input)  : the highest color index to use for images
C--
C 1994-Mar-17 : new routine [AFT/TJP].
C---
      INCLUDE 'pgplot.inc'
      INTEGER IC1, IC2
C---
      CALL GRQCOL(IC1,IC2)
      PGMNCI(PGID) = MIN(IC2,MAX(IC1,ICILO))
      PGMXCI(PGID) = MIN(IC2,MAX(IC1,ICIHI))
C
      END
