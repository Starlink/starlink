*+  PSF_QAXIS - Retrieve axis data from internal storage
      SUBROUTINE PSF_QAXIS( SLOT, AX, DIM, REG, PTR, BASE, SCALE, LABEL,
     :                      UNITS, TOR, STATUS )
*
*    Description :
*
*     Gets axis information for dataset defined by user
*
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     15 Dec 93 : Original (DJA)
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
      INTEGER                 SLOT              ! Psf handle
      INTEGER                 AX                ! Axis number
*
*    Export :
*
      INTEGER                 DIM		! Axis dimension
      LOGICAL                 REG               ! Axis is spaced?
      INTEGER                 PTR		! Pointer to non-spaced data
      REAL                    BASE, SCALE       ! Spaced axis attributes
      REAL                    TOR               ! Radian conversion factor
      CHARACTER*(*)           LABEL             ! Label
      CHARACTER*(*)           UNITS             ! Units
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Use internal routine to get data
      CALL PSF1_GETAXVAL( P_INST(SLOT), AX, DIM, REG, PTR, BASE, SCALE,
     :                    TOR, STATUS )
      CALL PSF1_GETAXTXT( P_INST(SLOT), AX, LABEL, UNITS, STATUS )

      END
