*+  ARR_SUM1R - returns sum of real array
      SUBROUTINE ARR_SUM1R(N,ARRAY,SUM,STATUS)
*    Description :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER N
      REAL ARRAY(*)
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
          SUM=SUM+ARRAY(I)
        ENDDO

      ENDIF

      END
