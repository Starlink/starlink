*+  ARR_MEANRQ - Finds mean and standard deviation of array, with quality
      SUBROUTINE ARR_MEANRQ( N, ARRAY, QUAL, MEAN, SDEV, STATUS )
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
      LOGICAL               QUAL(N)
*
*    Export :
*
      REAL                  MEAN, SDEV
*
*    Local variables :
*
      INTEGER               I, NGOOD
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      MEAN = 0.0
      NGOOD = 0
      DO I = 1,N
         IF ( QUAL(I) ) THEN
            MEAN = MEAN + ARRAY(I)
            NGOOD = NGOOD + 1
         END IF
      END DO

      MEAN = MEAN / NGOOD

      SDEV = 0.0
      DO I = 1,N
         IF ( QUAL(I) ) THEN
            SDEV = SDEV + ( ARRAY(I) - MEAN ) ** 2.0
         END IF
      END DO

      SDEV = SQRT( SDEV / NGOOD )

      END
