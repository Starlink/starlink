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
      INCLUDE 'AST_PKG'
*
*    Status :
*
      INTEGER STATUS
*-

*  Reset status
      STATUS = SAI__OK

*  System is now shut down
      CALL AST_CPKGI( PSF__PKG )

*  Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_CLOSE', STATUS )
      END IF

      END
