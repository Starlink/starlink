C*PGSVP -- set viewport (normalized device coordinates)
C%void cpgsvp(float xleft, float xright, float ybot, float ytop);
C+
      SUBROUTINE PGSVP (XLEFT, XRIGHT, YBOT, YTOP)
      REAL XLEFT, XRIGHT, YBOT, YTOP
C
C Change the size and position of the viewport, specifying
C the viewport in normalized device coordinates.  Normalized
C device coordinates run from 0 to 1 in each dimension. The
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
C  XLEFT  (input)  : x-coordinate of left hand edge of viewport, in NDC.
C  XRIGHT (input)  : x-coordinate of right hand edge of viewport,
C                    in NDC.
C  YBOT   (input)  : y-coordinate of bottom edge of viewport, in NDC.
C  YTOP   (input)  : y-coordinate of top  edge of viewport, in NDC.
C--
C 13-Dec-1990  Make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
      REAL     XS, YS
C
      IF (PGNOTO('PGSVP'))  RETURN
      IF (XLEFT.GE.XRIGHT .OR. YBOT.GE.YTOP) THEN
          CALL GRWARN('PGSVP ignored: invalid arguments')
          RETURN
      END IF
C
      XS = PGXSZ(PGID)/PGXPIN(PGID)
      YS = PGYSZ(PGID)/PGYPIN(PGID)
      CALL PGVSIZ(XLEFT*XS, XRIGHT*XS, YBOT*YS, YTOP*YS)
      END
