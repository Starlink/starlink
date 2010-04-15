      SUBROUTINE JTY_ADDPHASE(N,CFN,PHASE)
C History:  Original version J. Tonry.
C Modified: TDCA 18-FEB-1999. Minor style changes.

      IMPLICIT NONE

      COMPLEX*8 CFN(1), FACTOR
      REAL PHASE, ARG
      INTEGER J, N

      DO J = 1,N
          IF(J.LT.N/2+1) THEN
              ARG = (J-1) * PHASE
          ELSE
              ARG = (J-N-1) * PHASE
          END IF
          FACTOR = CMPLX(COS(ARG),SIN(ARG))
          CFN(J) = CFN(J) * FACTOR
      END DO
      RETURN
      END
