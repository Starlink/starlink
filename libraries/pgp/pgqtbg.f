C*PGQTBG -- inquire text background color index
C%void cpgqtbg(int *tbci);
C+
      SUBROUTINE PGQTBG (TBCI)
      INTEGER  TBCI
C
C Query the current Text Background Color Index (set by routine
C PGSTBG).
C
C Argument:
C  TBCI   (output) : receives the current text background color index.
C--
C 16-Oct-1993 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQTBG')) THEN
          TBCI = 0
      ELSE
          TBCI = PGTBCI(PGID)
      END IF
      END
