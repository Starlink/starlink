C*PGBBUF -- begin batch of output (buffer)
C%void cpgbbuf(void);
C+
      SUBROUTINE PGBBUF
C
C Begin saving graphical output commands in an internal buffer; the
C commands are held until a matching PGEBUF call (or until the buffer
C is emptied by PGUPDT). This can greatly improve the efficiency of
C PGPLOT.  PGBBUF increments an internal counter, while PGEBUF
C decrements this counter and flushes the buffer to the output
C device when the counter drops to zero.  PGBBUF and PGEBUF calls
C should always be paired.
C
C Arguments: none
C--
C 21-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (.NOT.PGNOTO('PGBBUF')) THEN
          PGBLEV(PGID) = PGBLEV(PGID) + 1
      END IF
      END
