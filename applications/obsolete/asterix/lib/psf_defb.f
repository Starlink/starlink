*+  PSF_DEFB - Define time and energy band for a psf by BIN number
      SUBROUTINE PSF_DEFB( SLOT, ALOWT, AHIGHT, ALOWE, AHIGHE, USERIN,
     :                                               USEROUT, STATUS )
*
*    Description :
*
*     Defines the current time and/or energy slice by bin number. This
*     is only sensible to call with a binned dataset.
*
*    Method :
*
*     Simply calls PSF_DEF with values derived from the dataset axis
*     attributes.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      4 Jan 94 : Copied from PSF_DEF (DJA)
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
      INTEGER                  SLOT                    	! The PSF to use
      DOUBLE PRECISION         ALOWT, AHIGHT            ! Time band
      INTEGER                  ALOWE, AHIGHE            ! Energy band
      INTEGER                  USERIN                  	! User extras in
*
*    Export :
*
      INTEGER                  USEROUT                 	! User stuff out
*
*    Status :
*
      INTEGER                  STATUS
*
*    Local variables :
*
      DOUBLE PRECISION         LOWT, HIGHT             	! Time band

      REAL		       AXVAL			! Axis value
      REAL                     BASE, SCALE, TOR		! Axis attibutes

      INTEGER	               APTR			! Axis irregular data
      INTEGER	               DIM			! Axis dimension
      INTEGER                  LOWE, HIGHE             	! Energy band
      INTEGER                  X_AX, Y_AX, E_AX, T_AX	! Axis identifiers

      LOGICAL                  REG			! Regular axis?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that this is an event dataset
      IF ( P_EVDS(SLOT) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Cannot define time and/or energy bands by'/
     :                     /'bin number for event dataset', STATUS )
        GOTO 99
      END IF

*    Get the defined axes for this psf
      CALL PSF_QAXES( SLOT, X_AX, Y_AX, E_AX, T_AX, STATUS )

*    Time axis defined?
      IF ( T_AX .GT. 0 ) THEN
        CALL PSF1_GETAXVAL( P_INST(SLOT), T_AX, DIM, REG, APTR, BASE,
     :                      SCALE, TOR, STATUS )
        IF ( REG ) THEN
          LOWT = BASE + (ALOWT-1)*SCALE
          HIGHT = BASE + (AHIGHT-1)*SCALE
        ELSE
          CALL ARR_ELEM1R( APTR, DIM, ALOWE, AXVAL, STATUS )
          LOWT = DBLE(AXVAL)
          CALL ARR_ELEM1R( APTR, DIM, AHIGHE, AXVAL, STATUS )
          HIGHT = DBLE(AXVAL)
        END IF
      ELSE
        LOWT = 0.0D0
        HIGHT = 0.0D0
      END IF

*    Energy axis defined?
      IF ( E_AX .GT. 0 ) THEN
        CALL PSF1_GETAXVAL( P_INST(SLOT), E_AX, DIM, REG, APTR, BASE,
     :                      SCALE, TOR, STATUS )
        IF ( REG ) THEN
          LOWE = BASE + (ALOWE-1)*SCALE
          HIGHE = BASE + (AHIGHE-1)*SCALE
        ELSE
          CALL ARR_ELEM1R( APTR, DIM, ALOWE, AXVAL, STATUS )
          LOWE = NINT(AXVAL)
          CALL ARR_ELEM1R( APTR, DIM, AHIGHE, AXVAL, STATUS )
          HIGHE = NINT(AXVAL)
        END IF
      ELSE
        LOWE = 0
        HIGHE = 0
      END IF

*    Is a definition routine ddefined?
      CALL PSF_DEF( SLOT, LOWT, HIGHT, LOWE, HIGHE, USERIN, USEROUT,
     :              STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_DEFB', STATUS )
      END IF

      END
