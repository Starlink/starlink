*+  PSF_INIT - Initialise the PSF system
      SUBROUTINE PSF_INIT( STATUS )
*
*    Description :
*
*     Sets up PSF_CMN
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     25 Oct 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    External references :
*
      EXTERNAL 			PSF_BLK
*
*    Local variables :
*
      INTEGER                  	I               ! Loop over libraries/models
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Zero the library slots
      L_NLIB = 0

*    Reset psf slots
      DO I = 1, PSF_NMAX
        P_USED(I) = .FALSE.
      END DO

*    Reset default time/energy block
      TE_INIT = .FALSE.

*    Psf system now initialised
      PSFINIT = .TRUE.

      END
