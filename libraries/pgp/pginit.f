C PGINIT -- initialize PGPLOT (internal routine)
C
      SUBROUTINE PGINIT
C
C Initialize PGPLOT. This routine should be called once during program
C execution, before any other PGPLOT routines.
C--
C Last modified: 1996 Apr 30 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER CALLED, I
      SAVE CALLED
      DATA CALLED /0/
C
      IF (CALLED.EQ.0) THEN
         PGID = 0
         DO 10 I=1,PGMAXD
            PGDEVS(I) = 0
 10      CONTINUE
         CALL GRINIT
         CALLED = 1
      END IF
C
      RETURN
      END
