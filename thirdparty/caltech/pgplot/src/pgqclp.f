C*PGQCLP -- inquire clipping status
C%void cpgqclp(int *state);
C+
      SUBROUTINE PGQCLP(STATE)
      INTEGER  STATE
C
C Query the current clipping status (set by routine PGSCLP).
C
C Argument:
C  STATE  (output) : receives the clipping status (0 => disabled,
C                    1 => enabled).
C--
C 25-Feb-1997 [TJP] - new routine.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQCLP')) THEN
         STATE = 1
      ELSE
         STATE = PGCLP(PGID)
      END IF
      END
