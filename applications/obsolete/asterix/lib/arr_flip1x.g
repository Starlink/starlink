*+  ARR_FLIP1<T> - Flip one dimensional array of <TYPE>
      SUBROUTINE ARR_FLIP1<T>( N, IN, OUT, STATUS )
*
*    Description :
*
*    History :
*
*     11 Dec 89 : Original ( BHVAD::DJA )
*     11 Jul 94 : Converted to GENERIC routine (DJA)
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
      INTEGER N
      <TYPE> IN(N)
*
*    Export :
*
      <TYPE> OUT(N)
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER I,J
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      J = N
      DO I = 1, N
         OUT(J) = IN(I)
         J = J - 1
      END DO

      END
