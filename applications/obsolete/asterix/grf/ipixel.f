*+  IPIXEL - display pixels for current image
      SUBROUTINE IPIXEL(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      26 Jan 93: V1.7-0
*       1 Jul 94: V1.7-1 screen clearing more intelligent (RJV)
*       1 Sep 94: V1.7-2 QUALITY option added (RJV)
*       7 Sep 94: V1.7-3 OFF option added (RJV)
*      15 Dec 94: V1.8-0 REG option added (RJV)
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
      LOGICAL FRESH
      LOGICAL QUAL,REG
      LOGICAL OFF
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IPIXEL Version 1.8-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)


      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSE

        CALL USI_GET0L('OFF',OFF,STATUS)

        IF (OFF) THEN
          QUAL=.FALSE.
          REG=.FALSE.
        ELSE
          CALL USI_GET0L('QUAL',QUAL,STATUS)
          IF (QUAL) THEN
            REG=.FALSE.
          ELSE
            CALL USI_GET0L('REG',REG,STATUS)
          ENDIF
        ENDIF

        IF (QUAL.AND..NOT.I_QOK) THEN
          CALL MSG_PRNT('AST_ERR: no QUALITY available for plotting')

        ELSEIF (REG.AND.I_REG_TYPE.EQ.'NONE') THEN
          CALL MSG_PRNT('AST_ERR: no region currently defined')

        ELSE

          CALL GCB_ATTACH('IMAGE',STATUS)
          CALL IMG_2DGCB(STATUS)

          IF (OFF) THEN
            CALL GCB_CANL('PIX_FLAG',STATUS)

          ELSE
*  clear screen unless freshly opened device
            CALL GDV_FRESH(FRESH,STATUS)
            IF (.NOT.FRESH) THEN
              CALL GDV_CLEAR(STATUS)
            ENDIF


*  display image
            CALL IMG_WINDOW(STATUS)
            IF (QUAL) THEN
              CALL IMG_QPIXEL(STATUS)
            ELSEIF (REG) THEN
              CALL IMG_RPIXEL(STATUS)
            ELSE
              CALL IMG_PIXEL(STATUS)
            ENDIF
            CALL IMG_AXES(STATUS)

*  flag current plotting status
            IF (.NOT.QUAL) THEN
              CALL GCB_SETL('PIX_FLAG',.TRUE.,STATUS)
              CALL GCB_SETL('SURF_FLAG',.FALSE.,STATUS)
            ENDIF
            I_DISP=.TRUE.
            I_DISP_1D=.FALSE.
            I_CLEAR=.FALSE.

          ENDIF

        ENDIF

      ENDIF

      CALL USI_CLOSE()

      END

