
C===========================================================================

      SUBROUTINE PERIOD_CONVOLV(M, A, M1, A1, M2, A2, FREQ, INFO)

C===========================================================================
C Convolves complex arrays A1(0:M1) and A2(0:M2) to form A(0:M).
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992.
C
C Converted to Double Precision (KPD), August 2001
C Power-raising modified to use INTEGER power (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER M,M1,M2,I,J
      DOUBLE COMPLEX A(0:M), A1(0:M1), A2(0:M2), PERIOD_CVAL
      DOUBLE PRECISION FREQ(0:M)
      INTEGER LOOP, ISTEP, INFO
      DATA ISTEP/50/

C---------------------------------------------------------------------------
C Convolve A1 with A2 to form A.
C---------------------------------------------------------------------------

      LOOP = 0
      DO 100 J = 0, M
         A(J) = (0.0D0, 0.0D0)     ! Reset A(J).
         DO 50 I = -M2, M2
            A(J) = A(J) + PERIOD_CVAL(A2, I, M2)
     :             *PERIOD_CVAL(A1, J-I, M1)
 50      CONTINUE
         IF ( INFO.EQ.1 ) THEN
            IF ( J.EQ.(LOOP*ISTEP) ) THEN
               WRITE (*, 99001) FREQ(J), (CDABS(A(J)))**2
99001          FORMAT ('+Frequency =', D12.6, ',  CLEANed Power =',
     :                 D18.6)
               LOOP = LOOP + 1
            END IF
         END IF
 100  CONTINUE

      RETURN
      END
