*+  ARR_COP1<T> - Copies 1D <TYPE> array to another
      SUBROUTINE ARR_COP1<T>(N,IN,OUT,STATUS)
*    Description :
*    Method :
*    Authors :
*     (BHVAD::RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER N
      <TYPE> IN(*),OUT(*)
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER I
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO I=1,N
          OUT(I)=IN(I)
        ENDDO

      ENDIF
      END
