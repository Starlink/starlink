C.PGTIKL -- length of error bar terminal
C
      SUBROUTINE PGTIKL (T, XL, YL)
      REAL T, XL, YL
C
C Return the length of the terminal of an error bar, in world
C coordinates.
C
C Arguments:
C  T      (input)  : terminal multiplier
C  XL     (output) : terminal lnegth in world x-coordinates
C  YL     (output) : terminal lnegth in world y-coordinates
C--
C 31-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
C
      XL = T*PGXSP(PGID)*0.15/PGXSCL(PGID)
      YL = T*PGXSP(PGID)*0.15/PGYSCL(PGID)
C
      END
