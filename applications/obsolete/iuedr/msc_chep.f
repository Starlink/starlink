      SUBROUTINE MSC_CHEP( X, NTERM, TN )
*+
*
*   Name:
*      SUBROUTINE MSC_CHEP
*
*   Description:
*      Evaluate the Chebyshev Polynomials from 0 up to NTERM terms
*      ending in degree (NTERM-1).
*
*   History:
*      Jack Giddings      31-DEC-81     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      Use recurrence relation.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 X           ! x value (-1,+1)

      INTEGER NTERM      ! number of terms

*   Export:
      REAL*8 TN(NTERM)   ! Chebyshev function Tn(x)

*   Local variables:
      INTEGER I          ! recurrence cycle

      IF ( NTERM .GT. 2 ) THEN
         TN(1) = 1.0
         TN(2) = X
         DO I = 3, NTERM
            TN(I) = 2.0 * X * TN(I - 1) - TN(I - 2)
         END DO

      ELSE IF ( NTERM .EQ. 2 ) THEN
         TN(1) = 1.0
         TN(2) = X

      ELSE IF ( NTERM .EQ. 1 ) THEN
         TN(1) = 1.0
      END IF

      END
