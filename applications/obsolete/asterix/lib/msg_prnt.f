*+  MSG_PRNT - output a text string
      SUBROUTINE MSG_PRNT(STR)
*    Description :
*     Simpler invokation of MSG_OUT
*    Method :
*    Deficiencies :
*    Authors :
*     (BHVAD::RJV)
*    History :
*     24 Nov 88 : Original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
*    Import :
      CHARACTER*(*) STR
*    Import-Export :
*    Export :
*    Status :
*    Local variables :
      INTEGER ISTAT
*-
      ISTAT=SAI__OK
      CALL MSG_OUT(' ',STR,ISTAT)

      END
