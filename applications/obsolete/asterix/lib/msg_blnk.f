*+  MSG_BLNK - output a blank line
      SUBROUTINE MSG_BLNK()
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*     (BHVAD::RJV)
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
*    Import :
*    Import-Export :
*    Export :
*    Status :
*    Local variables :
      INTEGER ISTAT
*-
      ISTAT=SAI__OK
      CALL MSG_OUT(' ',' ',ISTAT)

      END
