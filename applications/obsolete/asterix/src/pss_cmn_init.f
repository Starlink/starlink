*+  PSS_CMN_INIT - Initialise (EV)PSS common block
      SUBROUTINE PSS_CMN_INIT( STATUS )
*
*    Description :
*
*     Zeros the PSS common block
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Mar 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise source storage
      CALL PSS_SRC_RESET( STATUS )

      END
