      INTEGER FUNCTION JTY_IBC(M,N)
C Returns the (m n) binomial coefficient

C History:  Original version J. Tonry.
C Modified: TDCA 18-FEB-1999. Minor style changes.

      IMPLICIT NONE

      INTEGER I, M, N

      JTY_IBC = 1
      DO I = M+1,N
          JTY_IBC = JTY_IBC * I
      ENDDO
      DO I = 2,N-M
          JTY_IBC = JTY_IBC / I
      ENDDO
      RETURN
      END
