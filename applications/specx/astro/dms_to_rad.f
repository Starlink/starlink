C-----------------------------------------------------------------------

      SUBROUTINE DMS_TO_RAD (IDEC, APDEC)

C  Routine to decode the declination as REAL*4 from integer array

      INTEGER   IDEC(4),KDEC(4)

      PI = 3.141593

      ISIGN = +1
      DO I = 1,4
        IF (IDEC(I).LT.0)   ISIGN = -1
        KDEC(I) = IABS(IDEC(I))
      END DO
      APDEC = 2.*PI*((((KDEC(4)/100.+KDEC(3))/60.+
     &                KDEC(2))/60.+KDEC(1))/360.)*ISIGN

      RETURN
      END


