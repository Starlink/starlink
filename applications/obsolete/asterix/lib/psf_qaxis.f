*+  PSF_QAXIS - Retrieve axis data from internal storage
      SUBROUTINE PSF_QAXIS( PSID, AX, DIM, REG, PTR, BASE, SCALE, LABEL,
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
*
*    Import :
*
      INTEGER                 PSID              ! Psf handle
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
      CALL PSF0_GETAXVAL( PSID, AX, DIM, REG, PTR, BASE, SCALE,
     :                    TOR, STATUS )
      CALL PSF0_GETAXTXT( PSID, AX, LABEL, UNITS, STATUS )

      END
