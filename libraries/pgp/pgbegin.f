C*PGBEGIN -- non-standard alias for PGBEG
C+
      INTEGER FUNCTION PGBEGIN (UNIT, FILE, NXSUB, NYSUB)
      INTEGER       UNIT
      CHARACTER*(*) FILE
      INTEGER       NXSUB, NYSUB
C
C See description of PGBEG.   
C--
      INTEGER       PGBEG
      PGBEGIN = PGBEG (UNIT, FILE, NXSUB, NYSUB)
      END
