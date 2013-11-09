*+  ARR_MEANR - Finds mean and standard deviation of array
      SUBROUTINE ARR_MEANR( N, ARRAY, MEAN, SDEV, STATUS )
*
*    History :
*     30 Aug 89 : ( BHVAD :: DJA )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER               STATUS
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER               N
      REAL                  ARRAY(N)
*
*    Export :
*
      REAL                  MEAN, SDEV
*
*    Local variables :
*
      INTEGER               I
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      MEAN = 0.0
      DO I = 1,N
         MEAN = MEAN + ARRAY(I)
      END DO

      MEAN = MEAN / N

      SDEV = 0.0
      DO I = 1,N
         SDEV = SDEV + ( ARRAY(I) - MEAN ) ** 2.0
      END DO

      SDEV = SQRT( SDEV / N )

      END
