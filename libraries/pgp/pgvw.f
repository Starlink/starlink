C
      SUBROUTINE PGVW
C
C PGPLOT (internal routine): set the GRPCKG scaling transformation
C and window appropriate for the current window and viewport. This
C routine is called whenever the viewport or window is changed.
C
C Arguments: none
C
C (11-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
C Scale plotter in world coordinates.
C
      PGXSCL(PGID) = PGXLEN(PGID)/ABS(PGXTRC(PGID)-PGXBLC(PGID))
      PGYSCL(PGID) = PGYLEN(PGID)/ABS(PGYTRC(PGID)-PGYBLC(PGID))
      IF (PGXBLC(PGID).GT.PGXTRC(PGID)) THEN
          PGXSCL(PGID) = -PGXSCL(PGID)
      END IF
      IF (PGYBLC(PGID).GT.PGYTRC(PGID)) THEN
          PGYSCL(PGID) = -PGYSCL(PGID)
      END IF
      PGXORG(PGID) = PGXOFF(PGID)-PGXBLC(PGID)*PGXSCL(PGID)
      PGYORG(PGID) = PGYOFF(PGID)-PGYBLC(PGID)*PGYSCL(PGID)
      CALL GRTRN0(PGXORG(PGID),PGYORG(PGID),
     1            PGXSCL(PGID),PGYSCL(PGID))
C
C Window plotter in viewport.
C
      CALL GRAREA(PGID,PGXOFF(PGID),PGYOFF(PGID),
     1            PGXLEN(PGID),PGYLEN(PGID))
      END
