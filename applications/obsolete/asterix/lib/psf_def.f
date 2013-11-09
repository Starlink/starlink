*+  PSF_DEF - Define time and energy band for a psf - also user stuff
      SUBROUTINE PSF_DEF( PSID, LOWT, HIGHT, LOWE, HIGHE, USERIN,
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
*     30 Oct 1989 (DJA):
*        Original version
*     14 Mar 1996 (DJA):
*        No longer a psf method, just set instance variables directly
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
      INTEGER                  PSID                    ! The PSF to use
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

*  Check inherited status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set instance variables
      CALL ADI_CPUT0D( PSID, 'TimeLo', LOWT, STATUS )
      CALL ADI_CPUT0D( PSID, 'TimeHi', HIGHT, STATUS )
      CALL ADI_CPUT0L( PSID, 'TimeDef', .TRUE., STATUS )
      CALL ADI_CPUT0I( PSID, 'PhaLo', LOWE, STATUS )
      CALL ADI_CPUT0I( PSID, 'PhaHi', HIGHE, STATUS )
      CALL ADI_CPUT0L( PSID, 'PhaDef', .TRUE., STATUS )

*  Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_DEF', STATUS )
      END IF

      END
