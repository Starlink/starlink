*+  PSF_QENERGY - Is the dataset associated with a psf energy dependent?
      SUBROUTINE PSF_QENERGY( SLOT, ENERGY, STATUS )
*
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     28 Jun 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER                  SLOT                    ! Psf handle
*
*    Export :
*
      LOGICAL                  ENERGY                  ! Dataset is energy dep't
*
*    Status :
*
      INTEGER                  STATUS
*
*
*
      INTEGER                  X,Y,E,T			! Axis identifiers
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get axis id's
      CALL PSF_QAXES( SLOT, X, Y, E, T, STATUS )

*    Psf associated data has energy data available
      ENERGY = ( E .NE. 0 )

      END
