*+  GCB_CONNECT - establish connection with Grafix Control Block
      SUBROUTINE GCB_CONNECT(STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
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
