*+  TCHOP - chop up time series into segments
      SUBROUTINE TCHOP(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*       May 94: V1.7-0 (RJV)
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
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TCHOP Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.T_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: time-series system not active')
      ELSE

        CALL USI_INIT()

        CALL GCB_ATTACH('TIME',STATUS)

        IF (T_CHOPPED) THEN
          CALL MSG_PRNT('AST_ERR: time series already chopped')
        ELSE
          CALL TIM_CHOP(%VAL(T_QPTR),STATUS)
          CALL TIM_SCALE(%VAL(T_APTR),%VAL(T_WPTR),STATUS)
          CALL GDV_CLEAR(STATUS)
          CALL TIM_PLOT(STATUS)
          IF (STATUS.EQ.SAI__OK) THEN
            T_CHOPPED=.TRUE.
            T_DISP=.TRUE.
            T_CLEAR=.FALSE.
          ENDIF
        ENDIF

        CALL USI_CLOSE()
      ENDIF

      END
