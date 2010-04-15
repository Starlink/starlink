
C===========================================================================

      FUNCTION PERIOD_DFOUR(N, TIME, DATA, FREQ)

C===========================================================================
C Returns the Fourier transform of the time series specified by TIME(1:N)
C and DATA(1:N), evaluated at the frequency FREQ. The FT is normalized to
C have the data mean at DC.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992.
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE "PIVARS"

      INTEGER N,K
      DOUBLE PRECISION FREQ, DATA(N), TIME(N)
      DOUBLE PRECISION PHASE
      DOUBLE COMPLEX PERIOD_DFOUR
      DOUBLE COMPLEX FT

C---------------------------------------------------------------------------
C Evaluate FT at FREQ.
C---------------------------------------------------------------------------

      FT = (0.0D0, 0.0D0)

      DO 100 K = 1, N
         PHASE = -TWOPI*FREQ*TIME(K)
         FT = FT + DATA(K)*DCMPLX(DCOS(PHASE), DSIN(PHASE))
 100  CONTINUE

C---------------------------------------------------------------------------
C Return with FT properly normalized.
C---------------------------------------------------------------------------

      PERIOD_DFOUR = FT/DFLOAT(N)

      RETURN
      END
