C*PGNCURSE -- non-standard alias for PGNCUR
C+
      SUBROUTINE PGNCURSE (MAXPT, NPT, X, Y, SYMBOL)
      INTEGER MAXPT, NPT
      REAL    X(*), Y(*)
      INTEGER SYMBOL
C
C See description of PGNCUR.
C--
      CALL PGNCUR (MAXPT, NPT, X, Y, SYMBOL)
      END
