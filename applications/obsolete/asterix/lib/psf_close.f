*+  PSF_CLOSE - Shutdown the PSF system
      SUBROUTINE PSF_CLOSE( )
*
*    Description :
*
*     Deactivates every shareable image which has been initialised.
*
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     07 Nov 89 : Original (DJA)
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
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER PSF_FINDR
*
*    Local variables :
*
      INTEGER                   I                 ! Loop over libraries/psfs
      INTEGER                   CLOSE_PTR         ! Library closure routine
*-

*    Reset status
      STATUS = SAI__OK

*    Release any active psfs
      DO I = 1, PSF_NMAX
        IF ( P_USED(I) ) THEN
          CALL PSF_RELEASE( I, STATUS )
        END IF
      END DO

*    Loop over all psf libraries
      DO I = 1, L_NLIB

*      Look for shareable image shutdown routine
        STATUS = PSF_FINDR( 'PSF_SHARE_CLOSE', I, CLOSE_PTR )
        IF ( STATUS .EQ. SAI__ERROR ) THEN
          STATUS = SAI__OK
        ELSE
          CALL PSF_LIB_CLOSE_EXEC( %VAL(CLOSE_PTR), STATUS )
        END IF

      END DO

*    System is now shut down
      PSFINIT = .FALSE.

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_CLOSE', STATUS )
      END IF

      END



*+  PSF_LIB_CLOSE_EXEC - Close down a shareable image
      SUBROUTINE PSF_LIB_CLOSE_EXEC( ROUTINE, STATUS )
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     07 Nov 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      EXTERNAL                 ROUTINE              ! Closure routine
*
*    Status :
*
      INTEGER STATUS
*-

      CALL ROUTINE( STATUS )

      END
