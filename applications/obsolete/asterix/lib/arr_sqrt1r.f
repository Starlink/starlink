*+  ARR_SQRT1R - Takes square root of all components of real array
      SUBROUTINE ARR_SQRT1R(X,N,STATUS)
*    Description :
*    Authors :
*           (BH750::RJV)
*    History :
*        Original 18TH MAR 88
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
          IF (X(I).GE.0.0) THEN
            X(I)=SQRT(X(I))
          ELSE
            X(I)=0.0
          END IF
        ENDDO
      END IF
      END
