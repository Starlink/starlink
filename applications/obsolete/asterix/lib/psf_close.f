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
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                   I                 ! Loop over libraries/psfs
*-

*  Reset status
      STATUS = SAI__OK

*  Release any active psfs
      DO I = 1, PSF_NMAX
        IF ( P_USED(I) ) THEN
          CALL PSF_RELEASE( I, STATUS )
        END IF
      END DO

*  System is now shut down
      PSFINIT = .FALSE.

*  Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_CLOSE', STATUS )
      END IF

      END
