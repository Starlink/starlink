C*PGQAH -- inquire arrow-head style
C%void cpgqah(int *fs, float *angle, float *vent);
C+
      SUBROUTINE PGQAH (FS, ANGLE, VENT)
      INTEGER  FS
      REAL ANGLE, VENT
C
C Query the style to be used for arrowheads drawn with routine PGARRO.
C
C Argument:
C  FS     (output) : FS = 1 => filled; FS = 2 => outline.
C  ANGLE  (output) : the acute angle of the arrow point, in degrees.
C  VENT   (output) : the fraction of the triangular arrow-head that
C                    is cut away from the back.
C--
C 13-Oct-1992 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      FS = PGAHS(PGID)
      ANGLE = PGAHA(PGID)
      VENT = PGAHV(PGID)
C
      END
