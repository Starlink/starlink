*+  PSF_DEF - Define time and energy band for a psf - also user stuff
      SUBROUTINE PSF_DEF( SLOT, LOWT, HIGHT, LOWE, HIGHE, USERIN,
     :                                            USEROUT, STATUS )
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Oct 89 : Original (DJA)
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
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      INTEGER                  SLOT                    ! The PSF to use
      DOUBLE PRECISION         LOWT, HIGHT             ! Time band
      INTEGER                  LOWE, HIGHE             ! Energy band
      INTEGER                  USERIN                  ! User extras in
*
*    Export :
*
      INTEGER                  USEROUT                 ! User stuff out
*
*    Status :
*
      INTEGER                  STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Is a definition routine ddefined?
      IF ( L_MOD_DEF(P_MODID(SLOT),P_LIBID(SLOT)) .NE. 0 ) THEN

        CALL PSF_DEF_EXEC( %VAL(L_MOD_DEF(P_MODID(SLOT),P_LIBID(SLOT))),
     :                     SLOT, LOWT, HIGHT,
     :                     LOWE, HIGHE, USERIN, USEROUT, STATUS )

      ELSE

*      Do some global definition
        TE_TLO = LOWT
        TE_THI = HIGHT
        TE_ELO = LOWE
        TE_EHI = HIGHE
        TE_INIT = .TRUE.

      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_DEF', STATUS )
      END IF

      END



*+  PSF_DEF_EXEC - Define time and energy band for a psf - also user stuff
      SUBROUTINE PSF_DEF_EXEC( ROUTINE, SLOT, LOWT, HIGHT, LOWE,
     :                          HIGHE, USERIN, USEROUT, STATUS )
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Oct 89 : Original (DJA)
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
      INTEGER                  SLOT                    ! The PSF to use
      DOUBLE PRECISION         LOWT, HIGHT             ! Time band
      INTEGER                  LOWE, HIGHE             ! Energy band
      INTEGER                  USERIN                  ! User extras in
      EXTERNAL                 ROUTINE                 ! The _DEF routine
*
*    Export :
*
      INTEGER                  USEROUT                 ! User stuff out
*
*    Status :
*
      INTEGER                  STATUS
*-

      CALL ROUTINE( SLOT, LOWT, HIGHT, LOWE, HIGHE, USERIN, USEROUT,
     :                                                      STATUS )

      END
