      REAL FUNCTION JTY_GETRMS(N,X)
C Compute the rms of a complex buffer and set it to RMS
C Assume a transform of a real function

C History:  Original version J. Tonry.
C Modified: TDCA 18-FEB-1999. Minor style changes.

      IMPLICIT NONE

      COMPLEX*8 X(1)
      INTEGER N, J

      JTY_GETRMS = CABS(X(1))*CABS(X(1))+CABS(X(N/2+1))*CABS(X(N/2+1))
      DO J = 2,N/2
          JTY_GETRMS = JTY_GETRMS + 2 * CABS(X(J)) * CABS(X(J))
      END DO
      JTY_GETRMS = SQRT(JTY_GETRMS) / N ! since it is the transform
      RETURN
      END
