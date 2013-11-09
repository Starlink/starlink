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
      INCLUDE 'AST_PKG'
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
      EXTERNAL			PSF1_ASCA_INI
      EXTERNAL			PSF1_EXLE_INI
      EXTERNAL			PSF1_PWFC_INI
      EXTERNAL			PSF_RADIAL_INIT
      EXTERNAL			PSF_RESPFILE_INIT
      EXTERNAL			PSF_TABULAR_INIT
      EXTERNAL			PSF1_WFC_INI
      EXTERNAL			PSF1_RHRI_INI
      EXTERNAL			PSF_XRT_PSPC_INIT
      EXTERNAL                  PSF_XMM_ANAL_INIT
      EXTERNAL                  PSF_AXAF_INIT
      EXTERNAL                  PSF_AXAF_S_INIT
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Load the ADI package
      CALL ADI_REQPKG( 'psf', STATUS )

*  Create the psf store
      CALL ADI_NEW0( 'STRUC', P_PLIST, STATUS )

*  Load the system psfs
      CALL PSF0_DEFPSF( 'ANALYTIC', PSF_ANAL_INIT, STATUS )
      CALL PSF0_DEFPSF( 'ASCA', PSF1_ASCA_INI, STATUS )
      CALL PSF0_DEFPSF( 'EXOLE', PSF1_EXLE_INI, STATUS )
      CALL PSF0_DEFPSF( 'PWFC', PSF1_PWFC_INI, STATUS )
      CALL PSF0_DEFPSF( 'RADIAL', PSF_RADIAL_INIT, STATUS )
      CALL PSF0_DEFPSF( 'RESPFILE', PSF_RESPFILE_INIT, STATUS )
      CALL PSF0_DEFPSF( 'TABULAR', PSF_TABULAR_INIT, STATUS )
      CALL PSF0_DEFPSF( 'WFC', PSF1_WFC_INI, STATUS )
      CALL PSF0_DEFPSF( 'XRT_HRI', PSF1_RHRI_INI, STATUS )
      CALL PSF0_DEFPSF( 'XRT_PSPC', PSF_XRT_PSPC_INIT, STATUS )
      CALL PSF0_DEFPSF( 'XMM_ANAL', PSF_XMM_ANAL_INIT,STATUS)
      CALL PSF0_DEFPSF( 'AXAF_MARX', PSF_AXAF_INIT,STATUS)
      CALL PSF0_DEFPSF( 'AXAF_MARX_S', PSF_AXAF_S_INIT, STATUS)
*  Now initialised
      CALL AST_SPKGI( PSF__PKG )

      END
