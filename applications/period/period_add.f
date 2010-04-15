
C===========================================================================

      SUBROUTINE PERIOD_ADD(M, A, M1, A1, M2, A2)

C===========================================================================
C Adds complex arrays A1(0:M1) and A2(0:M2) to form A(0:M).
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992.
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER M,M1,M2,J
      DOUBLE COMPLEX A(0:M), A1(0:M1), A2(0:M2), PERIOD_CVAL

C---------------------------------------------------------------------------
C Add A1 to A2, forming A.
C---------------------------------------------------------------------------

      DO 100 J = 0, M
         A(J) = PERIOD_CVAL(A1, J, M1) + PERIOD_CVAL(A2, J, M2)
 100  CONTINUE

      RETURN
      END
