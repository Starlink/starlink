*+  TLOAD - start up interactive time-series processing system
      SUBROUTINE TLOAD(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*     4 Mar 94 : v1.7-0 updated to G* graphics
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
      CHARACTER*(DAT__SZLOC) ILOC
      CHARACTER*80 TSD
      CHARACTER*30 DEV
      INTEGER NX,NY
      LOGICAL ACTIVE
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'Tload Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (T_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: time-series system already active')
      ELSE

*  general initialisation
        CALL AST_INIT()

*  get input image
        CALL PAR_GET0C('INP',TSD,STATUS)
        CALL HDS_OPEN(TSD,'UPDATE',ILOC,STATUS)

        CALL MSG_PRNT(' ')

        IF (STATUS.EQ.SAI__OK) THEN

          CALL MSG_PRNT('Checking time-series...')
          CALL TIM_CHECK(ILOC,STATUS)

          CALL MSG_PRNT('Mapping time-series....')
          CALL TIM_MAP(ILOC,STATUS)

*  default to whole time series
          CALL TIM_NOCHOP(STATUS)
          CALL TIM_SCALE(%VAL(T_APTR),%VAL(T_WPTR),STATUS)

        ENDIF

        CALL GCB_ATTACH('TIME',STATUS)

        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (ACTIVE) THEN
          CALL GDV_DEVICE(DEV,STATUS)
        ELSE
          CALL PAR_GET0C('DEV',DEV,STATUS)
          CALL PAR_GET0I('NX',NX,STATUS)
          CALL PAR_GET0I('NY',NY,STATUS)
          CALL GDV_OPEN(DEV,NX,NY,STATUS)
        ENDIF

        CALL PAR_GET0I('MODE',T_MODE,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN
          T_DEV=DEV
          T_OPEN=.TRUE.
          T_XZONES=NX
          T_YZONES=NY
          T_DISP=.FALSE.
          T_CHOPPED=.FALSE.
          T_CLEAR=.TRUE.
          T_LOC=ILOC
          IF (T_VOK) THEN
            T_ERR=.TRUE.
            T_POLY=.FALSE.
          ELSE
            T_POLY=.TRUE.
            T_ERR=.FALSE.
          ENDIF
          T_STEP=.FALSE.
          T_SYMBOL=0
        ELSE
          CALL GDV_CLOSE(STATUS)
          CALL GCB_DETACH(STATUS)
          T_OPEN=.FALSE.
          CALL AST_CLOSE()
          CALL HDS_CLOSE(ILOC,STATUS)
        ENDIF

      ENDIF

      END
