      SUBROUTINE MSC_LUSLV(M, N, A, B)

*+
*
*   Name:
*      SUBROUTINE MSC_LUSLV
*
*   Description:
*      Solution of linear equations.
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
      DOUBLE PRECISION B(M)        ! rhs and result

*   LU reduce A-matrix
      CALL MSC_LURED(M, N, A)

*   Resolve for B-vector
      CALL MSC_RESLV(M, N, A, B)

      END
