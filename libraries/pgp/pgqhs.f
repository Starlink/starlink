C*PGQHS -- inquire hatching style
C%void cpgqhs(float *angle, float *sepn, float* phase);
C+
      SUBROUTINE PGQHS (ANGLE, SEPN, PHASE)
      REAL ANGLE, SEPN, PHASE
C
C Query the style to be used hatching (fill area with fill-style 3).
C
C Arguments:
C  ANGLE  (output) : the angle the hatch lines make with the
C                    horizontal, in degrees, increasing 
C                    counterclockwise (this is an angle on the
C                    view surface, not in world-coordinate space).
C  SEPN   (output) : the spacing of the hatch lines. The unit spacing
C                    is 1 percent of the smaller of the height or
C                    width of the view surface.
C  PHASE  (output) : a real number between 0 and 1; the hatch lines
C                    are displaced by this fraction of SEPN from a
C                    fixed reference.  Adjacent regions hatched with the
C                    same PHASE have contiguous hatch lines.
C--
C 26-Feb-1995 - new routine [TJP].
C 19-Jun-1995 - correct synopsis [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      ANGLE = PGHSA(PGID)
      SEPN  = PGHSS(PGID)
      PHASE = PGHSP(PGID)
C
      END
