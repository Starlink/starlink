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
*    Functions:
      INTEGER CHR_LEN
*    Local variables :
      INTEGER L
      INTEGER ISTAT
*-
      ISTAT=SAI__OK
      L=CHR_LEN(STR)
      CALL MSG_OUT(' ',STR(1:L),ISTAT)

      END
