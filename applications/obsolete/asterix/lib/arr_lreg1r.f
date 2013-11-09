*+  ARR_LREG1R - Create an array regularly spaced in log value
      SUBROUTINE ARR_LREG1R( LOG_BASE, LOG_SCALE, N, ARRAY, STATUS )
*
*    Description :
*
*     Sets ARRAY(i) = 10**(LOG_BASE+(i-1)*LOG_SCALE)
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Jun 92 : Original (DJA)
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
      INTEGER                  N                       ! # values to do
      REAL                     LOG_BASE, LOG_SCALE     !
*
*    Export :
*
      REAL                     ARRAY(*)                !
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                  I                       ! Loop over values
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, N
        ARRAY(I) = 10.0**(REAL(I-1)*LOG_SCALE+LOG_BASE)
      END DO

      END
