*+  IUNDO - undo last changes to image
      SUBROUTINE IUNDO(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IUNDO Version 1.7-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_CAN_UNDO) THEN
        CALL MSG_PRNT('AST_ERR: cannot undo')
      ELSE

        CALL MSG_PRNT('Undoing '//I_LAST_CMD)
        CALL IMG_SWAP(STATUS)
        I_CAN_UNDO=.FALSE.
        I_PROC_COUNT=I_PROC_COUNT-1
        I_LAST_CMD=' '


      ENDIF

      CALL USI_CLOSE()

      END


