C-----------------------------------------------------------------------

      SUBROUTINE FINDMX (NBPTS, SCEN, BUF, BADVAL, TMAX)

C  Routine to find position of maximum in scan. Simplified just to
C  search for maximum value.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   NBPTS
      REAL      SCEN
      REAL      BUF(*)
      REAL      BADVAL
      REAL      TMAX

*     Local variables:

      INTEGER   J
      INTEGER   JMAX

*     Functions:

*  Ok, go...

      JMAX = 1
      DO WHILE (JMAX.LE.NBPTS .AND. BUF(JMAX).EQ.BADVAL)
        JMAX = JMAX + 1
      END DO

      IF (JMAX.GT.NBPTS) THEN
        TMAX = BADVAL

      ELSE IF (JMAX.EQ.NBPTS) THEN
        TMAX = BUF(JMAX)

      ELSE

        TMAX = BUF(JMAX)

        DO J = JMAX+1, NBPTS
          IF (BUF(J).NE.BADVAL) THEN
            IF (BUF(J).GT.TMAX) THEN
              JMAX = J
              TMAX = BUF(J)
            END IF
          END IF
        END DO

      END IF

      SCEN = JMAX

      RETURN
      END


