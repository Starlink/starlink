*+  IMODE - switch mode
      SUBROUTINE IMODE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      21 SEP 90 :V1.2-1 able to set mode explicitly as well as toggle (RJV)
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
      LOGICAL KEY,CURS
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IMODE Version 1.2-1')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSE
        CALL PAR_GET0L('KEY',KEY,STATUS)
        IF (.NOT.KEY) THEN
          CALL PAR_GET0L('CURS',CURS,STATUS)
        ENDIF

*  toggle mode
        IF (.NOT.(KEY.OR.CURS)) THEN
          IF (I_MODE.EQ.1) THEN
            I_MODE=2
          ELSEIF (I_MODE.EQ.2) THEN
            I_MODE=1
          ENDIF

*  set specific mode
        ELSEIF (KEY) THEN
          I_MODE=2
        ELSEIF (CURS) THEN
          I_MODE=1
        ENDIF

        CALL MSG_PRNT(' ')
        IF (I_MODE.EQ.2) THEN
          CALL MSG_PRNT('Mode set to KEYBOARD mode')
        ELSE
          CALL MSG_PRNT('Mode set to CURSOR mode')
        ENDIF
        CALL MSG_PRNT(' ')

      ENDIF

      END
