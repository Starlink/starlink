C*PGPANL -- switch to a different panel on the view surface
C%void cpgpanl(int nxc, int nyc);
C+
      SUBROUTINE PGPANL(IX, IY)
      INTEGER IX, IY
C
C Start plotting in a different panel. If the view surface has been
C divided into panels by PGBEG or PGSUBP, this routine can be used to
C move to a different panel. Note that PGPLOT does not remember what
C viewport and window were in use in each panel; these should be reset
C if necessary after calling PGPANL. Nor does PGPLOT clear the panel:
C call PGERAS after calling PGPANL to do this.
C
C Arguments:
C  IX     (input)  : the horizontal index of the panel (in the range
C                    1 <= IX <= number of panels in horizontal
C                    direction).
C  IY     (input)  : the vertical index of the panel (in the range
C                    1 <= IY <= number of panels in horizontal
C                    direction).
C--
C  1-Dec-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE      'pgplot.inc'
      LOGICAL PGNOTO
C
C Check that a device is open.
C
      IF (PGNOTO('PGPANL')) RETURN
C
C Check arguments.
C
      IF (IX.LT.1 .OR. IX.GT.PGNX(PGID) .OR.
     :    IY.LT.1 .OR. IY.GT.PGNY(PGID)) THEN
         CALL GRWARN('PGPANL: the requested panel does not exist')
C
C Adjust the viewport to the new panel and window the plot
C in the new viewport.
C
      ELSE
         PGNXC(PGID)  = IX
         PGNYC(PGID)  = IY
         PGXOFF(PGID) = PGXVP(PGID) + (IX-1)*PGXSZ(PGID)
         PGYOFF(PGID) = PGYVP(PGID) + (PGNY(PGID)-IY)*PGYSZ(PGID)
         CALL PGVW
      END IF
C
      END
