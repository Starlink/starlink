*+  FIT_DEFREGRID - Define values in a regular grid axis block
      SUBROUTINE FIT_DEFREGRID( PAR, NVAL, LOGARITHMIC, BASE, SCALE,
     :                                                 IAX, STATUS )
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
c     RECORD /GRID_AXIS/       GAX                     ! Grid axis block
      INTEGER                  IAX
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Fill in fields
      GRID_AXIS_PAR(IAX) = PAR
      GRID_AXIS_REGULAR(IAX) = .TRUE.
      GRID_AXIS_NVAL(IAX) = NVAL
      GRID_AXIS_LOGARITHMIC(IAX) = LOGARITHMIC
      GRID_AXIS_BASE(IAX) = BASE
      GRID_AXIS_SCALE(IAX) = SCALE

      END
