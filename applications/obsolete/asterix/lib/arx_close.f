*+  ARX_CLOSE - close ARD text
      SUBROUTINE ARX_CLOSE(ID,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global Variables :
*    Import :
      INTEGER ID
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Local Constants :
*    Local variables :
*-

      CALL GRP_DELET(ID,STATUS)


      END
