*+  IWHOLE - select whole image
      SUBROUTINE IWHOLE(STATUS)
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
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Local Constants :
*    Local variables :
*    Global Variables :
      INCLUDE 'IMG_CMN'
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='IWHOLE Version 1.2-0')
*-

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSE

        CALL IMG_SETWHOLE(STATUS)

        CALL MSG_PRNT(' ')
        CALL MSG_PRNT('Whole image selected...')
        CALL MSG_PRNT(' ')

      ENDIF

      END
