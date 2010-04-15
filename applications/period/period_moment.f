
      SUBROUTINE PERIOD_MOMENT(DATA, N, AVE, ADEV, SDEV, VAR)

C===============================================================================
C Given an array of DATA of length N, this routine returns its mean AVE,
C average deviation ADEV, standard deviation SDEV and variance VAR.
C
C Adapted from Numerical Recipes by Vikram Singh Dhillon @Sussex 29-April-1992.
C
C Converted to Double Precision (KPD), August 2001
C===============================================================================

      IMPLICIT NONE

C-------------------------------------------------------------------------------
C PERIOD_MOMENT declarations.
C-------------------------------------------------------------------------------

      INTEGER          N,J
      DOUBLE PRECISION AVE,ADEV,SDEV,VAR
      DOUBLE PRECISION S,DATA(N)

      IF ( N.LE.1 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: N must be at least 2 in PERIOD_MOMENT.'
         GO TO 300
      END IF

C-------------------------------------------------------------------------------
C First pass to get the mean.
C-------------------------------------------------------------------------------

      S = 0.0D0
      DO 100 J = 1, N
         S = S + DATA(J)
 100  CONTINUE
      AVE = S/DFLOAT(N)

C-------------------------------------------------------------------------------
C Second pass to get the first (absolute) and second moments of the deviation
C from the mean.
C-------------------------------------------------------------------------------

      ADEV = 0.0D0
      VAR = 0.0D0
      DO 200 J = 1, N
         S = DATA(J) - AVE
         ADEV = ADEV + DABS(S)
         VAR = VAR + S*S
 200  CONTINUE

C-------------------------------------------------------------------------------
C Put the pieces together according to the conventional definitions.
C-------------------------------------------------------------------------------

      ADEV = ADEV/DFLOAT(N)
      VAR = VAR/DFLOAT(N-1)
      SDEV = DSQRT(VAR)

 300  CONTINUE
      RETURN
      END
