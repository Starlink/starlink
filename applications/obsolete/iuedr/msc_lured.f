      SUBROUTINE MSC_LURED(M, N, A)

*+
*
*   Name:
*      SUBROUTINE MSC_LURED
*
*   Description:
*      LU decomposition of matrix.
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

*   Import-Export:
      DOUBLE PRECISION A(M, M)     ! matrix operator

*   Local variables:
      INTEGER I                    ! block index
      INTEGER IP1                  ! number of lines to be reduced
      INTEGER K                    ! line index
      INTEGER J                    ! column index
      INTEGER NM1                  ! number of blocks to be reduced

      DOUBLE PRECISION FACT        ! normalisation factor

      IF (N.GT.1) THEN

         NM1 = N - 1

         DO 50 I = 1, NM1

            IP1 = I + 1

            DO 20 K = IP1, N

               FACT = A(K, I)/A(I, I)

               DO 10 J = IP1, N

                  A(K, J) = A(K, J) - A(I, J)*FACT

 10            CONTINUE

 20         CONTINUE

 50      CONTINUE

      END IF

      END
