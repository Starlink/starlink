C*PGSAH -- set arrow-head style
C%void cpgsah(int fs, float angle, float barb);
C+
      SUBROUTINE PGSAH (FS, ANGLE, BARB)
      INTEGER  FS
      REAL ANGLE, BARB
C
C Set the style to be used for arrowheads drawn with routine PGARRO.
C
C Argument:
C  FS     (input)  : FS = 1 => filled; FS = 2 => outline.
C                    Other values are treated as 2. Default 1.
C  ANGLE  (input)  : the acute angle of the arrow point, in degrees;
C                    angles in the range 20.0 to 90.0 give reasonable
C                    results. Default 45.0.
C  BARB   (input)  : the fraction of the triangular arrow-head that
C                    is cut away from the back. 0.0 gives a triangular
C                    wedge arrow-head; 1.0 gives an open >. Values 0.3
C                    to 0.7 give reasonable results. Default 0.3.
C--
C 13-Oct-1992 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      PGAHS(PGID) = FS
      IF (PGAHS(PGID).NE.1) PGAHS(PGID) = 2
      PGAHA(PGID) = ANGLE
      PGAHV(PGID) = BARB
C
      END
