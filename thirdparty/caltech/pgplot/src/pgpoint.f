C*PGPOINT -- non-standard alias for PGPT
C+
      SUBROUTINE PGPOINT (N, XPTS, YPTS, SYMBOL)
      INTEGER N
      REAL XPTS(*), YPTS(*)
      INTEGER SYMBOL
C
C See description of PGPT.
C--
      CALL PGPT (N, XPTS, YPTS, SYMBOL)
      END
