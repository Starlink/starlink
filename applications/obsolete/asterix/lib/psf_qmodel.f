*+  PSF_QMODEL - Is the psf handle a psf model specification
      SUBROUTINE PSF_QMODEL( PSID, MODEL, STATUS )
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
*
*    Import :
*
      INTEGER                  PSID                    ! Psf handle
*
*    Export :
*
      LOGICAL                  MODEL                   ! Is psf a model?
*
*    Status :
*
      INTEGER                  STATUS
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Is the psf a model?
      CALL ADI_CGET0L( PSID, 'IsModel', MODEL, STATUS )

      END
