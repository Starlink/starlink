C*PGSHS -- set hatching style
C%void cpgshs(float angle, float sepn, float phase);
C+
      SUBROUTINE PGSHS (ANGLE, SEPN, PHASE)
      REAL ANGLE, SEPN, PHASE
C
C Set the style to be used for hatching (fill area with fill-style 3).
C The default style is ANGLE=45.0, SEPN=1.0, PHASE=0.0.
C
C Arguments:
C  ANGLE  (input)  : the angle the hatch lines make with the
C                    horizontal, in degrees, increasing 
C                    counterclockwise (this is an angle on the
C                    view surface, not in world-coordinate space).
C  SEPN   (input)  : the spacing of the hatch lines. The unit spacing
C                    is 1 percent of the smaller of the height or
C                    width of the view surface. This should not be
C                    zero.
C  PHASE  (input)  : a real number between 0 and 1; the hatch lines
C                    are displaced by this fraction of SEPN from a
C                    fixed reference.  Adjacent regions hatched with the
C                    same PHASE have contiguous hatch lines. To hatch
C                    a region with alternating lines of two colors,
C                    fill the area twice, with PHASE=0.0 for one color
C                    and PHASE=0.5 for the other color.
C--
C 26-Feb-1995 - new routine [TJP].
C 12-Feb-1996 - check for zero spacing [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSHS')) RETURN
      PGHSA(PGID) = ANGLE
      IF (SEPN.EQ.0.0) THEN
         CALL GRWARN('PGSHS: zero hatch line spacing requested')
         PGHSS(PGID) = 1.0
      ELSE
         PGHSS(PGID) = SEPN
      END IF
      IF (PHASE.LT.0.0 .OR. PHASE.GT.1.0) THEN
         CALL GRWARN('PGSHS: hatching phase must be in (0.0,1.0)')
      END IF
      PGHSP(PGID) = PHASE
C
      END
