C*PGVSIZ -- set viewport (inches)
C%void cpgvsiz(float xleft, float xright, float ybot, float ytop);
C+
      SUBROUTINE PGVSIZ (XLEFT, XRIGHT, YBOT, YTOP)
      REAL XLEFT, XRIGHT, YBOT, YTOP
C
C Change the size and position of the viewport, specifying
C the viewport in physical device coordinates (inches).  The
C viewport is the rectangle on the view surface "through"
C which one views the graph.  All the PG routines which plot lines
C etc. plot them within the viewport, and lines are truncated at
C the edge of the viewport (except for axes, labels etc drawn with
C PGBOX or PGLAB).  The region of world space (the coordinate
C space of the graph) which is visible through the viewport is
C specified by a call to PGSWIN.  It is legal to request a
C viewport larger than the view surface; only the part which
C appears on the view surface will be plotted.
C
C Arguments:
C  XLEFT  (input)  : x-coordinate of left hand edge of viewport, in
C                    inches from left edge of view surface.
C  XRIGHT (input)  : x-coordinate of right hand edge of viewport, in
C                    inches from left edge of view surface.
C  YBOT   (input)  : y-coordinate of bottom edge of viewport, in
C                    inches from bottom of view surface.
C  YTOP   (input)  : y-coordinate of top  edge of viewport, in inches
C                    from bottom of view surface.
C--
C 13-Dec-1990  Make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
C
      IF (PGNOTO('PGVSIZ'))  RETURN
      IF (XLEFT.GE.XRIGHT .OR. YBOT.GE.YTOP) THEN
          CALL GRWARN('PGVSIZ ignored: invalid arguments')
          RETURN
      END IF
C
      PGXLEN(PGID) = (XRIGHT-XLEFT)*PGXPIN(PGID)
      PGYLEN(PGID) = (YTOP-YBOT)*PGYPIN(PGID)
      PGXVP(PGID)  = XLEFT*PGXPIN(PGID)
      PGYVP(PGID)  = YBOT*PGYPIN(PGID)
      PGXOFF(PGID) = PGXVP(PGID) + (PGNXC(PGID)-1)*PGXSZ(PGID)
      PGYOFF(PGID) = PGYVP(PGID) + 
     1                (PGNY(PGID)-PGNYC(PGID))*PGYSZ(PGID)
      CALL PGVW
      END
