*+  MSG_PRNT - output a text string
      SUBROUTINE MSG_PRNT(STR)
*    Description :
*     Simpler invokation of MSG_OUT
*    Method :
*    Deficiencies :
*    Authors :
*     (BHVAD::RJV)
*     (Birmingham: RB)
*    History :
*     24 Nov 88 : Original
*      7 Apr 98 : Don't use MSG_OUT is its rubbish (RB)
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
      CHARACTER*256 BUF
      INTEGER BLEN
      INTEGER ISTAT
*-
      ISTAT=SAI__OK

*    Translate any tokens
      CALL MSG_LOAD( ' ', STR, BUF, BLEN, ISTAT )

*    Write to the full width screen
      WRITE( 6, '(A)' ) BUF(:BLEN)

      END
