*+  ARR_SUM1I - returns REAL sum of integer array
      SUBROUTINE ARR_SUM1I(N,ARRAY,SUM,STATUS)
*    Description :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER N
      INTEGER ARRAY(*)
*    Import-Export :
*    Export :
      REAL SUM
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER I
*-

      IF (STATUS.EQ.SAI__OK) THEN

        SUM=0.0
        DO I=1,N
          SUM=SUM+REAL(ARRAY(I))
        ENDDO

      ENDIF

      END
