*+  GCB_LCONNECT - connect to local GCB in dynamic memory
      SUBROUTINE GCB_LCONNECT(STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-
      CALL GCB_ATTACH('LOCAL',STATUS)

      END
