*+  PSF_QAXEXT - Retrieve axis extrema from internal storage
      SUBROUTINE PSF_QAXEXT( SLOT, AX, LO, HI, STATUS )
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
*      5 Jan 94 : Original (DJA)
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
      REAL                    LO, HI		! Axis extrema
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL			LO2
      INTEGER                 DIM		! Axis dimension
      LOGICAL                 REG               ! Axis is spaced?
      INTEGER                 PTR		! Pointer to non-spaced data
      REAL                    BASE, SCALE       ! Spaced axis attributes
      REAL                    TOR               ! Radian conversion factor
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Use internal routine to get data
      CALL PSF1_GETAXVAL( P_INST(SLOT), AX, DIM, REG, PTR, BASE, SCALE,
     :                    TOR, STATUS )

*    Regular axis?
      IF ( REG ) THEN
        LO = BASE - SCALE*0.5
        HI = LO + REAL(DIM)*SCALE
      ELSE
        CALL ARR_ELEM1R( PTR, DIM, 1, LO, STATUS )
        CALL ARR_ELEM1R( PTR, DIM, DIM, HI, STATUS )
        IF ( DIM .GT. 2 ) THEN
          CALL ARR_ELEM1R( PTR, DIM, 2, LO2, STATUS )
        ELSE
          LO2 = HI
        END IF
        LO = LO - (LO2-LO)/2.0
        HI = HI + (LO2-LO)/2.0
      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_QAXEXT', STATUS )
      END IF

      END
