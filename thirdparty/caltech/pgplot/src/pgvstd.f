C*PGVSTD -- set standard (default) viewport
C%void cpgvstd(void);
C+
      SUBROUTINE PGVSTD
C
C Define the viewport to be the standard viewport.  The standard
C viewport is the full area of the view surface (or panel),
C less a margin of 4 character heights all round for labelling.
C It thus depends on the current character size, set by PGSCH.
C
C Arguments: none.
C--
C 22-Apr-1983: [TJP].
C  2-Aug-1995: [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
      REAL     XLEFT, XRIGHT, YBOT, YTOP, R
C
      IF (PGNOTO('PGVSIZ')) RETURN
C
      R = 4.0*PGYSP(PGID)
      XLEFT  = R/PGXPIN(PGID)
      XRIGHT = XLEFT + (PGXSZ(PGID)-2.0*R)/PGXPIN(PGID)
      YBOT   = R/PGYPIN(PGID)
      YTOP   = YBOT + (PGYSZ(PGID)-2.0*R)/PGYPIN(PGID)
      CALL PGVSIZ(XLEFT, XRIGHT, YBOT, YTOP)
      END
