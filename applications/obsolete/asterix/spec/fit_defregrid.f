*+  FIT_DEFREGRID - Define values in a regular grid axis block
      SUBROUTINE FIT_DEFREGRID( PAR, NVAL, LOGARITHMIC, BASE, SCALE,
     :                                                 GAX, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Jul 92 : Original (DJA)
*      7 Apr 93 : Removed limit on number of grid values (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      INTEGER                  PAR                     ! Parameter number
      INTEGER                  NVAL                    ! # values in axis
      LOGICAL                  LOGARITHMIC             ! Log axis?
      REAL                     BASE, SCALE             ! Origin and spacing
*
*    Export :
*
      RECORD /GRID_AXIS/       GAX                     ! Grid axis block
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Fill in fields
      GAX.PAR = PAR
      GAX.REGULAR = .TRUE.
      GAX.NVAL = NVAL
      GAX.LOGARITHMIC = LOGARITHMIC
      GAX.BASE = BASE
      GAX.SCALE = SCALE

      END
