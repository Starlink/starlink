*+  ARR_SQR1R - Squares all components of real array
      SUBROUTINE ARR_SQR1R(X,N,STATUS)
*    Description :
*    Authors :
*           (BH750::RJV)
*    History :
*        Original 29th Jan 88
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status definition :
      INTEGER STATUS
*    Import :
      INTEGER N
*    Import/Export :
      REAL X(N)
      INTEGER I
*-
      IF (STATUS.EQ.SAI__OK) THEN
        DO I=1,N
          X(I)=X(I)*X(I)
        ENDDO
      END IF
      END
