*+  TSTYLE - set plotting style
      SUBROUTINE TSTYLE(STATUS)
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
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER SYMBOL
      LOGICAL POLY,STEP,ERR
      LOGICAL SET
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TSTYLE Version 1.4-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.T_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: time series system not active')

      ELSE

        CALL USI_INIT()

        SET=.FALSE.

*  get style specification
        CALL USI_GET0L('POLY',POLY,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          POLY=T_POLY
          STATUS=SAI__OK
        ELSE
          SET=.TRUE.
        ENDIF

        CALL USI_GET0L('STEP',STEP,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          STEP=T_STEP
          STATUS=SAI__OK
        ELSE
          SET=.TRUE.
        ENDIF

        CALL USI_GET0L('ERR',ERR,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          ERR=T_ERR
          STATUS=SAI__OK
        ELSE
          SET=.TRUE.
        ENDIF

        CALL USI_GET0I('SYMBOL',SYMBOL,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          SYMBOL=T_SYMBOL
          STATUS=SAI__OK
        ELSE
          IF (SYMBOL.LT.0) THEN
            SYMBOL=0
          ENDIF
          SET=.TRUE.
        ENDIF


        IF (.NOT.SET) THEN
          CALL MSG_PRNT('AST_ERR: no style specified')
        ELSE
          T_POLY=POLY
          T_STEP=STEP
          T_ERR=ERR
          T_SYMBOL=SYMBOL
        ENDIF

        CALL USI_CLOSE()

      ENDIF

      END
