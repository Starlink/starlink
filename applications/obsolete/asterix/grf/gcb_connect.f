*+  GCB_CONNECT - establish connection with Grafix Control Block
      SUBROUTINE GCB_CONNECT(STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
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
