*+  GCB_DELCACHE - releases dynamic memory used for cache
      SUBROUTINE GCB_DELCACHE(PTR,STATUS)
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
      INTEGER PTR
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-


      CALL DYN_UNMAP(PTR,STATUS)


      END
