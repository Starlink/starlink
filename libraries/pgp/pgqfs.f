C*PGQFS -- inquire fill-area style
C%void cpgqfs(int *fs);
C+
      SUBROUTINE PGQFS (FS)
      INTEGER  FS
C
C Query the current Fill-Area Style attribute (set by routine
C PGSFS).
C
C Argument:
C  FS     (output) : the current fill-area style:
C                      FS = 1 => solid (default)
C                      FS = 2 => outline
C                      FS = 3 => hatched
C                      FS = 4 => cross-hatched
C--
C  5-Nov-1985 - new routine [TJP].
C  6-Mar-1995 - add styles 3 and 4 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQFS')) THEN
          FS = 1
      ELSE
          FS = PGFAS(PGID)
      END IF
      END
