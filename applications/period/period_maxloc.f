
C===========================================================================

      FUNCTION PERIOD_MAXLOC(M, ARRAY)

C===========================================================================
C Returns the array index of the ARRAY(0:M) element with maximum CABS value.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992.
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER PERIOD_MAXLOC
      INTEGER M,LMAX,I
      DOUBLE COMPLEX ARRAY(0:M)
      DOUBLE PRECISION AMAX,ARRAYI

C---------------------------------------------------------------------------
C Find the maximum location.
C---------------------------------------------------------------------------

      LMAX = 0
      AMAX = CDABS(ARRAY(0))
      DO 100 I = 1, M
         ARRAYI = CDABS(ARRAY(I))
         IF ( ARRAYI.GT.AMAX ) THEN
            AMAX = ARRAYI
            LMAX = I
         END IF
 100  CONTINUE
      PERIOD_MAXLOC = LMAX

      RETURN
      END
