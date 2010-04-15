      SUBROUTINE MSC_RESLV(M, N, A, B)

*+
*
*   Name:
*      SUBROUTINE MSC_RESLV
*
*   Description:
*      Resolve linear system of equations from LU reduced matrix.
*
*   History:
*      Jack Giddings      30-JUL-81     IUEDR Vn. 1.0
*      Paul Rees          28-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER M                    ! declared size of A-matrix and B-vector
      INTEGER N                    ! used size of A-matrix and B-vector

      DOUBLE PRECISION A(M, M)     ! matrix operator

*   Import-Export:
      DOUBLE PRECISION B(M)        ! rhs and result

*   Local variables:
      INTEGER IP1                  ! loop start
      INTEGER I                    ! block index
      INTEGER J                    ! loop index
      INTEGER K                    ! loop index
      INTEGER L                    ! loop index
      INTEGER NM1                  ! loop end

      IF (N.EQ.1) THEN

         B(N) = B(N)/A(N, N)

      ELSE

         NM1 = N - 1

         DO 50 I = 1, NM1

            IP1 = I + 1

            DO 20 J = IP1, N

               B(J) = B(J) - B(I)*A(J, I)/A(I, I)

 20         CONTINUE

 50      CONTINUE

         B(N) = B(N)/A(N, N)

         DO 100 I = 1, NM1

            K = N - I
            L = K + 1

            DO 60 J = L, N

               B(K) = B(K) - B(J)*A(K, J)

 60         CONTINUE

            B(K) = B(K)/A(K, K)

 100     CONTINUE

      END IF

      END
