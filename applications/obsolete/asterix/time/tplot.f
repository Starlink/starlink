*+  TPLOT - plot t/s
      SUBROUTINE TPLOT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*     May 94: V1.7-0
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'TIMLIB(TIM_CMN)'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TPLOT Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.T_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: time-series system not active')
      ELSE

        CALL GDV_CLEAR(STATUS)

        CALL GCB_ATTACH('TIME',STATUS)

        CALL TIM_PLOT(STATUS)

        IF (STATUS.EQ.SAI__OK) THEN
          T_DISP=.TRUE.
          T_CLEAR=.FALSE.
        ENDIF

      ENDIF

      END
