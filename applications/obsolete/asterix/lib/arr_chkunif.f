*+  ARR_CHKUNIF - Checks whether array has uniform values
      SUBROUTINE ARR_CHKUNIF(X,N,UNIF,VAL,STATUS)
*    Description :
*      Scans an array to see if all the values are the same
*      and if so returns the appropriate scalar value
*    Authors :
*           (BH750::RJV)
*    History :
*        Original 3/11/88
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status definition :
      INTEGER STATUS
*    Import :
      INTEGER N
      REAL X(N)
*    Import/Export :
*    Export :
      LOGICAL UNIF		! whether uniform
      REAL VAL			! scalar value
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN
        IF (N.GT.1) THEN
          VAL=X(1)
          I=1
          UNIF=.TRUE.
          DO WHILE (UNIF.AND.I.LT.N)
            J=I+1
            UNIF=(ABS(X(J)-X(I)).LT.0.00001)
            I=J
          ENDDO
          IF (.NOT.UNIF) THEN
            VAL=0.0
          ENDIF
        ELSE
          UNIF=.TRUE.
          VAL=X(1)
        ENDIF
      ENDIF
      END
