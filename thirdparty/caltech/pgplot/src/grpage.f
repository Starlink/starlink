C*GRPAGE -- end picture
C+
      SUBROUTINE GRPAGE
C
C GRPCKG: Advance the plotting area to a new page. For video devices,
C this amounts to erasing the screen; for hardcopy devices, the plot
C buffer is written to the output file followed by a form-feed to
C advance the paper to the start of the next page.
C
C Arguments: none.
C--
C  3-Jun-1983 - [TJP].
C 18-Feb-1984 - remove unnecessary 'T' initialization of VT125, and add
C               S(G1) for Rainbow REGIS [TJP].
C  1-Jun-1984 - add type GMFILE [TJP].
C  2-Jul-1984 - change initialization of VT125 for color [TJP].
C 13-Jul-1984 - move initialization of VT125 and Grinnell to GROPEN
C               [TJP].
C 19-Oct-1984 - add VV device [TJP].
C 29-Jan-1985 - add HP2648 terminal [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - fix GREXEC end picture sequence [AFT].
C 11-Jun-1987 - remove built-in devices [TJP].
C 11-Feb-1992 - update veiew surface size: it may have changed! [TJP].
C  5-Jan-1993 - but only if GRSETS has not been called! [TJP]
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      INTEGER NBUF,LCHR
      REAL    RBUF(6)
C
      CHARACTER CHR
C
C Flush the buffer.
C
      CALL GRTERM
C
C Erase the text screen (if there is one).
C
      CALL GRETXT
C
C End picture.
C
      CALL GREPIC
C
C Update the view surface size: it may have changed (on windowing 
C devices)
C
      IF (.NOT.GRADJU(GRCIDE)) THEN
          CALL GREXEC(GRGTYP, 6,RBUF,NBUF,CHR,LCHR)
          GRXMXA(GRCIDE) = RBUF(2)
          GRYMXA(GRCIDE) = RBUF(4)
      END IF
C
      END
