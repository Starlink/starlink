*+  SFIT_OPCLOSE - Close fitting o/p file
      SUBROUTINE SFIT_OPCLOSE( OCI, STATUS )
*
*    Description :
*
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Jan 93 : Original (DJA)
*     25 Jul 94 : Converted to use AIO system (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import / Export :
*
      INTEGER			OCI			! AIO stream id
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Close AIO stream
      CALL AIO_CLOSE( OCI, STATUS )

      END
