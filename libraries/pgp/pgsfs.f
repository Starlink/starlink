C*PGSFS -- set fill-area style
C%void cpgsfs(int fs);
C+
      SUBROUTINE PGSFS (FS)
      INTEGER  FS
C
C Set the Fill-Area Style attribute for subsequent area-fill by
C PGPOLY, PGRECT, or PGCIRC.  Four different styles are available: 
C solid (fill polygon with solid color of the current color-index), 
C outline (draw outline of polygon only, using current line attributes),
C hatched (shade interior of polygon with parallel lines, using
C current line attributes), or cross-hatched. The orientation and
C spacing of hatch lines can be specified with routine PGSHS (set
C hatch style).
C
C Argument:
C  FS     (input)  : the fill-area style to be used for subsequent
C                    plotting:
C                      FS = 1 => solid (default)
C                      FS = 2 => outline
C                      FS = 3 => hatched
C                      FS = 4 => cross-hatched
C                    Other values give an error message and are
C                    treated as 2.
C--
C 21-Oct-1985 - new routine [TJP].
C 17-Dec-1990 - pass to GR level [TJP].
C  6-Mar-1995 - add styles 3 and 4 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSFS')) RETURN
      IF (FS.LT.1 .OR. FS.GT.4) THEN
          CALL GRWARN('illegal fill-area style requested')
          PGFAS(PGID) = 2
      ELSE
          PGFAS(PGID) = FS
      END IF
      END
