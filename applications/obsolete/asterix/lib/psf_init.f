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
      EXTERNAL			PSF_ANAL_INIT
      EXTERNAL			PSF_ASCA_INIT
      EXTERNAL			PSF_EXOLE_INIT
      EXTERNAL			PSF_PWFC_INIT
      EXTERNAL			PSF_RADIAL_INIT
      EXTERNAL			PSF_RESPFILE_INIT
      EXTERNAL			PSF_TABULAR_INIT
      EXTERNAL			PSF_WFC_INIT
      EXTERNAL			PSF_XRT_HRI_INIT
      EXTERNAL			PSF_XRT_PSPC_INIT
*
*    Local variables :
*
      INTEGER                  	I               ! Loop over libraries/models
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reset psf slots
      DO I = 1, PSF_NMAX
        P_USED(I) = .FALSE.
      END DO

*  Reset default time/energy block
      TE_INIT = .FALSE.

*  Psf system now initialised
      PSFINIT = .TRUE.

*  Load the ADI package
      CALL ADI_REQPKG( 'psf', STATUS )

*  Create the psf store
      CALL ADI_NEW0( 'STRUC', P_PLIST, STATUS )

*  Load the system psfs
      CALL PSF0_DEFPSF( 'ANALYTIC', PSF_ANAL_INIT, STATUS )
      CALL PSF0_DEFPSF( 'ASCA', PSF_ASCA_INIT, STATUS )
      CALL PSF0_DEFPSF( 'EXOLE', PSF_EXOLE_INIT, STATUS )
      CALL PSF0_DEFPSF( 'PWFC', PSF_PWFC_INIT, STATUS )
      CALL PSF0_DEFPSF( 'RADIAL', PSF_RADIAL_INIT, STATUS )
      CALL PSF0_DEFPSF( 'TABULAR', PSF_TABULAR_INIT, STATUS )
      CALL PSF0_DEFPSF( 'WFC', PSF_WFC_INIT, STATUS )
      CALL PSF0_DEFPSF( 'XRT_HRI', PSF_XRT_HRI_INIT, STATUS )
      CALL PSF0_DEFPSF( 'XRT_PSPC', PSF_XRT_PSPC_INIT, STATUS )

      END
