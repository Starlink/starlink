*+  MATH_EPOLY - Evaulate a polynomial given by coeffs at given points
      SUBROUTINE MATH_EPOLY( N, DEGREE, COEFF, X, Y, STATUS )
*    Description :
*    Authors :
*
*     David J. Allan ( BHVAD::DJA )
*
*    History :
*
*     22 Nov 89 : Original ( DJA )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER       N, DEGREE
      REAL          COEFF(DEGREE+1),X(*)
*
*    Export :
*
      REAL          Y(*)
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER       I               ! Loop over requested values
      INTEGER       C               ! Loop over coefficients
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Evaluate at points given
      DO I = 1,N
         Y(I) = COEFF(1)
         IF ( DEGREE .GE. 1 ) THEN
            DO C = 2, DEGREE+1
               Y(I) = Y(I) + COEFF(C)*X(I)**(C-1)
            END DO
         END IF
      END DO

      END
