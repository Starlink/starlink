      SUBROUTINE READ_DMS (RA, PROMPT, ISTAT)

      IMPLICIT   NONE

      REAL*8     RA
      CHARACTER  PROMPT*(*)
      INTEGER    ISTAT

      REAL*8     XRA(3)
      INTEGER    I
      INTEGER    LP
      INTEGER    NRA
      INTEGER    ISIGN

      INTEGER    GEN_ILEN

*  Ok, go...

      ISTAT = 0
      ISIGN = 1

      LP    = GEN_ILEN (PROMPT)

      CALL GEN_GETR8A2 (PROMPT(:LP), XRA, 3,' ', XRA,  NRA,  ISTAT)
      IF (ISTAT.LT.0) RETURN

      IF (XRA(1).LT.0.0) ISIGN = -1

      DO I = 1, NRA
        XRA(4-I) = ABS (XRA(1+NRA-I))
      END DO

      IF (NRA.LT.3) THEN
        DO I = 1, 3-NRA
          XRA(I) = 0.0
        END DO
      END IF

      RA = ((XRA(3)/60. + XRA(2))/60. + XRA(1))
      RA = ISIGN*RA

      RETURN
      END
