*+  ARR_SUM1D - returns sum of double array
      SUBROUTINE ARR_SUM1D(N,ARRAY,SUM,STATUS)
*    Description :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER N
      DOUBLE PRECISION ARRAY(*)
*    Import-Export :
*    Export :
      DOUBLE PRECISION SUM
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER I
*-

      IF (STATUS.EQ.SAI__OK) THEN

        SUM=0.0D0
        DO I=1,N
          SUM=SUM+ARRAY(I)
        ENDDO

      ENDIF

      END
