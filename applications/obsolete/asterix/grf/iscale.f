*+  ISCALE - rescale image
      SUBROUTINE ISCALE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*     6 Feb 92: V1.2-1 display option added (RJV)
*    19 Jan 93: V1.7-0
*    18 Mar 93: V1.7-1 different scaling options added (RJV)
*     1 Jul 94: V1.7-2 more intelligent screen clearing (RJV)
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
      CHARACTER*10 SCALING
      INTEGER CYCLES
      LOGICAL DISP
      LOGICAL FRESH
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ISCALE Version 1.7-2')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSE

        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

        CALL USI_GET0C('SCALING',SCALING,STATUS)
        CALL CHR_UCASE(SCALING)
        CALL GCB_SETC('PIX_SCALING',SCALING,STATUS)
        IF (SCALING(:3).EQ.'CYC') THEN
          CALL USI_GET0I('CYCLES',CYCLES,STATUS)
          CALL GCB_SETI('PIX_CYCLES',CYCLES,STATUS)
        ENDIF

        CALL USI_DEF0R('MIN',I_DMIN,STATUS)
        CALL USI_DEF0R('MAX',I_DMAX,STATUS)
        CALL USI_GET0R('MIN',I_PMIN,STATUS)
        CALL USI_GET0R('MAX',I_PMAX,STATUS)
        CALL GCB_SETR('PIX_MIN',I_PMIN,STATUS)
        CALL GCB_SETR('PIX_MAX',I_PMAX,STATUS)

        CALL USI_GET0L('DISP',DISP,STATUS)
        IF (DISP.AND.STATUS.EQ.SAI__OK) THEN
*  clear plotting window unless device freshly opened
          CALL GDV_FRESH(FRESH,STATUS)
          IF (.NOT.FRESH) THEN
            CALL GDV_CLEAR(STATUS)
          ENDIF

*  display image
          CALL IMG_WINDOW(STATUS)
          CALL IMG_DISP(STATUS)
          CALL IMG_AXES(STATUS)


*  flag current plotting status
          CALL GCB_SETL('PIX_FLAG',.TRUE.,STATUS)
          CALL GCB_CANL('CONT_FLAG',STATUS)
          CALL GCB_CANL('SURF_FLAG',STATUS)
          I_DISP=.TRUE.
          I_DISP_1D=.FALSE.
          I_CLEAR=.FALSE.

        ENDIF

      ENDIF

      CALL USI_CLOSE()

      END


