*+  ARR_NORM1R  Normalise a data array by the sum
      SUBROUTINE ARR_NORM1R( N, DATA, STATUS )
*    Description :
*      Divides all elements of an array by their sum, if that sum is positive
*    History :
*      7 Jun 89 : Original ( DJA @ UK.AC.BHAM.SR.STAR )
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER   STATUS
*    Import :
      INTEGER   N
*    Import-Export :
      REAL      DATA(N)
*    Local variables :
      INTEGER      I
      REAL         SUM
*-
      IF ( STATUS .NE. SAI__OK ) RETURN

      SUM = 0.0
      DO I = 1, N
         SUM = SUM + DATA(I)

      END DO

      IF ( SUM .GT. 0.0 ) THEN
         DO I = 1, N
            DATA(I) = DATA(I) / SUM

         END DO

      ELSE
         CALL MSG_PRNT( 'Failed to normalise:'/
     :                /' integral under curve was less than 0')

      END IF

      END
