C*PGWNAD -- set window and adjust viewport to same aspect ratio
C%void cpgwnad(float x1, float x2, float y1, float y2);
C+
      SUBROUTINE PGWNAD (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C Change the window in world coordinate space that is to be mapped on
C to the viewport, and simultaneously adjust the viewport so that the
C world-coordinate scales are equal in x and y. The new viewport is
C the largest one that can fit within the previously set viewport
C while retaining the required aspect ratio.
C
C Arguments:
C  X1     (input)  : the x-coordinate of the bottom left corner
C                    of the viewport.
C  X2     (input)  : the x-coordinate of the top right corner
C                    of the viewport (note X2 may be less than X1).
C  Y1     (input)  : the y-coordinate of the bottom left corner
C                    of the viewport.
C  Y2     (input)  : the y-coordinate of the top right corner of the
C                    viewport (note Y2 may be less than Y1).
C--
C 25-Sep-1985 - new routine (TJP).
C 31-May-1989 - correct error: XVP and YVP not set (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL SCALE,OXLEN,OYLEN
C
      IF (PGNOTO('PGWNAD')) RETURN
C
C If invalid arguments are specified, issue warning and leave window
C unchanged.
C
      IF (X1.EQ.X2) THEN
         CALL GRWARN('invalid x limits in PGWNAD: X1 = X2.')
      ELSE IF (Y1.EQ.Y2) THEN
         CALL GRWARN('invalid y limits in PGWNAD: Y1 = Y2.')
      ELSE
         SCALE = MIN(PGXLEN(PGID)/ABS(X2-X1)/PGXPIN(PGID), 
     1               PGYLEN(PGID)/ABS(Y2-Y1)/PGYPIN(PGID))
         PGXSCL(PGID) = SCALE*PGXPIN(PGID)
         PGYSCL(PGID) = SCALE*PGYPIN(PGID)
         OXLEN = PGXLEN(PGID)
         OYLEN = PGYLEN(PGID)
         PGXLEN(PGID) = PGXSCL(PGID)*ABS(X2-X1)
         PGYLEN(PGID) = PGYSCL(PGID)*ABS(Y2-Y1)
         PGXVP(PGID)  = PGXVP(PGID) + 0.5*(OXLEN-PGXLEN(PGID))
         PGYVP(PGID)  = PGYVP(PGID) + 0.5*(OYLEN-PGYLEN(PGID))
         PGXOFF(PGID) = PGXVP(PGID) + (PGNXC(PGID)-1)*PGXSZ(PGID)
         PGYOFF(PGID) = PGYVP(PGID) +
     1                   (PGNY(PGID)-PGNYC(PGID))*PGYSZ(PGID)
         CALL PGSWIN(X1, X2, Y1, Y2)
      END IF
      END
