C*PGCURSE -- non-standard alias for PGCURS
C+
      INTEGER FUNCTION PGCURSE (X, Y, CH)
      REAL X, Y
      CHARACTER*1 CH
C
C See description of PGCURS.
C--
      INTEGER PGCURS
      PGCURSE = PGCURS (X, Y, CH)
      END
