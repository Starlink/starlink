C*PGSCLP -- enable or disable clipping at edge of viewport
C%void cpgsclp(int state);
C+
      SUBROUTINE PGSCLP(STATE)
      INTEGER STATE
C
C Normally all PGPLOT primitives except text are ``clipped'' at the
C edge of the viewport: parts of the primitives that lie outside
C the viewport are not drawn. If clipping is disabled by calling this
C routine, primitives are visible wherever they lie on the view
C surface. The default (clipping enabled) is appropriate for almost
C all applications.
C
C Argument:
C  STATE  (input)  : 0 to disable clipping, or 1 to enable clipping.
C 
C 25-Feb-1997 [TJP] - new routine.
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSCLP')) RETURN
C
C Disable clipping.
C
      IF (STATE.EQ.0) THEN
         CALL GRAREA(PGID,0.,0.,-1.,-1.)
         PGCLP(PGID) = 0
C
C Enable clipping.
C
      ELSE
         CALL GRAREA(PGID,PGXOFF(PGID),PGYOFF(PGID),
     :               PGXLEN(PGID),PGYLEN(PGID))
         PGCLP(PGID) = 1
      END IF
      END
