*+  TSELECT - select segments
      SUBROUTINE TSELECT(STATUS)
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
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*1 CH
      REAL XC,YC
      INTEGER SEL(T_MAXSECT),ISEL,NSEL
      LOGICAL ALL
      LOGICAL LEFT,RIGHT
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TSELECT Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.T_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: time-series system not active')
      ELSEIF (.NOT.T_DISP) THEN
        CALL MSG_PRNT('AST_ERR: time series must be plotted/re-plotted'
     :                           //' before using this command')

      ELSE

        CALL USI_INIT()

        CALL GCB_ATTACH('TIME',STATUS)

*  see if all segments to be selected
        CALL USI_GET0L('ALL',ALL,STATUS)
        IF (ALL.AND.STATUS.EQ.SAI__OK) THEN
          DO ISEL=1,T_NSECT
            SEL(ISEL)=ISEL
          ENDDO
          NSEL=T_NSECT

        ELSEIF (STATUS.EQ.SAI__OK) THEN
*  otherwise select segments individually
          NSEL=0
          CH=' '
          CALL MSG_PRNT('Select segments (press X to eXit)...')
          DO WHILE (CH.NE.'x'.AND.CH.NE.'X')
            CALL GFX_CURS(XC,YC,LEFT,RIGHT,CH,STATUS)
            IF (CH.NE.'x'.AND.CH.NE.'X') THEN
              NSEL=NSEL+1
              CALL TIM_CURSTOSECT(XC,SEL(NSEL),STATUS)
              IF (SEL(NSEL).EQ.0) THEN
                NSEL=NSEL-1
              ENDIF
            ENDIF
          ENDDO

        ENDIF

*  update selection list and rescale
        CALL ARR_INIT1L(.FALSE.,T_MAXSECT,T_SEL,STATUS)
        DO ISEL=1,NSEL
          T_SEL(SEL(ISEL))=.TRUE.
        ENDDO
        T_NSEL=NSEL
        CALL TIM_SCALE(%VAL(T_APTR),%VAL(T_WPTR),STATUS)
        CALL GDV_CLEAR(STATUS)
        CALL TIM_PLOT(STATUS)
        IF (STATUS.EQ.SAI__OK) THEN
          T_DISP=.TRUE.
          T_CLEAR=.FALSE.
        ENDIF

        CALL USI_CLOSE()

      ENDIF

      END
