*+  ARX_REWIND - reset ARD back to a specified index (line)
      SUBROUTINE ARX_REWIND(ID,INDEX,STATUS)
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
      INTEGER INDEX
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Local Constants :
*    Local variables :
*-

      CALL GRP_SETSZ(ID,INDEX,STATUS)


      END
