C*PGPTEXT -- non-standard alias for PGPTXT
C+
      SUBROUTINE PGPTEXT (X, Y, ANGLE, FJUST, TEXT)
      REAL X, Y, ANGLE, FJUST
      CHARACTER*(*) TEXT
C
C See description of PGPTXT.
C--
      CALL PGPTXT (X, Y, ANGLE, FJUST, TEXT)
      END
