*+  SSO_INIT - Initialise SSO system
      SUBROUTINE SSO_INIT( )
*
*    Description :
*
*     Resets use flags to make SSO resources (datasets and mapped items)
*     available to system.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Jul 91 : Original (DJA)
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
*    Global variables :
*
      INCLUDE 'SSO_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                    I                  ! Loop over SSO resources
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Reset use flags

*    for datasets
      DO I = 1, SSO__MXDS
        SSO.DS(I).USED = .FALSE.
      END DO

*    for mapped items
      DO I = 1, SSO__MXMI
        SSO.MI(I).USED = .FALSE.
      END DO

      END
