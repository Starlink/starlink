*+  MSG_BLNK - output a blank line
      SUBROUTINE MSG_BLNK()
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*     (BHVAD::RJV)
*     (Birmingham: RB)
*    History :
*      7 Apr 1998 : Don't use the useless MSG_OUT call
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
      WRITE( 6, * )

      END
