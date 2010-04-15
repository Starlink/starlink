

      FUNCTION PERIOD_GASDEV(ISET, IDUM)

C=============================================================================
C Returns a normally distributed deviate with zero mean and unit variance,
C using PERIOD_RAN1 as the source of uniform deviates. If ISET is odd this
C routine generates 2 uniform deviates using PERIOD_RAN1 and uses these to
C generate 2 random numbers from a Gaussian distribution. The first of these
C is used on odd calls. The second is used if ISET is even without any further
C calculation. IDUM is set to -1 to initialise PERIOD_RAN1.
C
C Adapted from Numerical Recipes by Vikram Singh Dhillon @Sussex 28-May-1991.
C
C Converted to Double Precision (KPD), August 2001
C=============================================================================

      IMPLICIT NONE

      DOUBLE PRECISION PERIOD_GASDEV
      DOUBLE PRECISION V1, V2, PERIOD_RAN1, R, FAC, GSET
      INTEGER ISET, IDUM

      SAVE GSET

      IF ( MOD(ISET,2).NE.0 ) THEN

 50      CONTINUE
         V1 = 2.0D0*PERIOD_RAN1(IDUM) - 1.0D0
         V2 = 2.0D0*PERIOD_RAN1(IDUM) - 1.0D0
         R = V1*V1 + V2*V2

         IF ( R.GE.1.0D0 ) GO TO 50

         FAC = DSQRT(-2.0D0*DLOG(R)/R)
         GSET = V1*FAC
         PERIOD_GASDEV = V2*FAC

      ELSE

         PERIOD_GASDEV = GSET

      END IF

      RETURN
      END
