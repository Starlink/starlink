C-----------------------------------------------------------------------

      SUBROUTINE RANGE (BUF, NBPTS, YMIN, YMAX)

C  Subroutine to determine range of values in BUF(NBPTS). Maximum value
C  is returned in YMAX, minimum in YMIN.

      IMPLICIT  NONE

C     Formal parameters:

      REAL    BUF(*)
      INTEGER NBPTS
      REAL    YMIN
      REAL    YMAX

C     Local variables;

      INTEGER I

C  Ok, go...

      YMIN = BUF(1)
      YMAX = BUF(1)
      DO I = 1, NBPTS
        IF (BUF(I).GT.YMAX)   YMAX = BUF(I)
        IF (BUF(I).LT.YMIN)   YMIN = BUF(I)
      END DO

      RETURN
      END


