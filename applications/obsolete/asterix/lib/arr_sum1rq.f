*+  ARR_SUM1RQ - returns sum of real array for points with good quality
      SUBROUTINE ARR_SUM1RQ(N,ARRAY,Q,MASK,SUM,STATUS)
*    Description :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Import :
      INTEGER N
      REAL ARRAY(*)
      BYTE Q(*)
      BYTE MASK
*    Import-Export :
*    Export :
      REAL SUM
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local variables :
      INTEGER I
*-

      IF (STATUS.EQ.SAI__OK) THEN

        SUM=0.0
        DO I=1,N
          IF (BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
            SUM=SUM+ARRAY(I)
          ENDIF
        ENDDO

      ENDIF

      END
