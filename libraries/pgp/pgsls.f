C*PGSLS -- set line style
C%void cpgsls(int ls);
C+
      SUBROUTINE PGSLS (LS)
      INTEGER  LS
C
C Set the line style attribute for subsequent plotting. This
C attribute affects line primitives only; it does not affect graph
C markers, text, or area fill.
C Five different line styles are available, with the following codes:
C 1 (full line), 2 (dashed), 3 (dot-dash-dot-dash), 4 (dotted),
C 5 (dash-dot-dot-dot). The default is 1 (normal full line).
C
C Argument:
C  LS     (input)  : the line-style code for subsequent plotting
C                    (in range 1-5).
C--
C  8-Aug-1985 - new routine, equivalent to GRSLS [TJP].
C  3-Jun-1984 - add GMFILE device [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSLS')) RETURN
      CALL GRSLS(LS)
      END
