*+  GDV_STATUS - check device status
      SUBROUTINE GDV_STATUS(ACTIVE,STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'GDV_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
      LOGICAL ACTIVE
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      ACTIVE=.FALSE.
      IF (STATUS.EQ.SAI__OK) THEN

        ACTIVE=(G_DEVICE.NE.' '.AND.G_DEVICE(1:1).NE.CHAR(0))

      ENDIF

      END
