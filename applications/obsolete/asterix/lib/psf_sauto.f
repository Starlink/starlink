*+  PSF_SAUTO
      SUBROUTINE PSF_SAUTO( AUTO, STATUS )
*
*    Description :
*
*     Sets PSF system automatic flag
*
*    Method :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     28 Jul 95 : Original (DJA)
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
      LOGICAL			AUTO			! Variable value
*
*    Status :
*
      INTEGER                   STATUS                  ! Run-time error
*
*    External references :
*
      EXTERNAL		       PSF_BLK
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set flag value
      PSFAUTO = AUTO

      END
