C*PGIDEN -- write username, date, and time at bottom of plot
C%void cpgiden(void);
C+
      SUBROUTINE PGIDEN
C
C Write username, date, and time at bottom of plot.
C
C Arguments: none.
C--
C  9-Feb-1988
C 10-Sep-1990 : adjust position of text [TJP]
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER L, M, CF, CI, LW
      CHARACTER*64 TEXT
      REAL D, CH
C
      CALL PGBBUF
C
C Get information for annotation.
C
      CALL GRUSER(TEXT, L)
      TEXT(L+1:) = ' '
      CALL GRDATE(TEXT(L+2:), M)
      L = L+1+M
C
C Save current attributes.
C
      CALL PGQCF(CF)
      CALL PGQCI(CI)
      CALL PGQLW(LW)
      CALL PGQCH(CH)
C
C Change attributes and write text.
C
      CALL PGSCF(1)
      CALL PGSCI(1)
      CALL PGSLW(1)
      CALL PGSCH(0.6)
      CALL GRLEN(TEXT(1:L),D)
      CALL GRTEXT(.FALSE., 0.0, .TRUE., PGXSZ(PGID)-D-2.0,
     1            2.0+PGYSZ(PGID)/130.0, TEXT(1:L))
C
C Restore attributes.
C
      CALL PGSCF(CF)
      CALL PGSCI(CI)
      CALL PGSLW(LW)
      CALL PGSCH(CH)
      CALL PGEBUF
C
      END
