*+  PSF_QMODEL - Is the psf handle a psf model specification
      SUBROUTINE PSF_QMODEL( SLOT, MODEL, STATUS )
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     14 Feb 91 : Original (DJA)
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
      INCLUDE 'ASTLIB(PSF_CMN)'
*
*    Import :
*
      INTEGER                  SLOT                    ! Psf handle
*
*    Export :
*
      LOGICAL                  MODEL                   ! Is psf a model?
*
*    Status :
*
      INTEGER                  STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Is the psf a model?
      MODEL = P_MODEL(SLOT)

      END
