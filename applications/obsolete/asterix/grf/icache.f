*+  ICACHE - various operations on cache image
      SUBROUTINE ICACHE(STATUS)
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
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Local Constants :
*    Local variables :
      CHARACTER*12 MODE
*    Global Variables :
      INCLUDE 'IMG_CMN'
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='ICACHE Version 2.1-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)


      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSE

        CALL USI_GET0C('MODE',MODE,STATUS)
        CALL CHR_UCASE(MODE)
        MODE=MODE(:3)

        IF (MODE.EQ.'POP') THEN
          IF (I_MEM) THEN
            CALL IMG_UNCACHE(STATUS)
            CALL GCB_ATTACH('IMAGE',STATUS)
            CALL IMG_2DGCB(STATUS)
            I_DISP=.FALSE.
          ELSE
            CALL MSG_PRNT('AST_ERR: no image currently in cache')
          ENDIF
        ELSEIF (MODE.EQ.'PUS') THEN
          CALL GCB_ATTACH('IMAGE',STATUS)
          CALL IMG_2DGCB(STATUS)
          CALL IMG_GETCACHE(I_VOK,I_QOK,STATUS)
          CALL IMG_CACHE(STATUS)
          I_MEM=.TRUE.
          IF (I_GUI) THEN
            CALL IMG_NBPUT0I('CACHE',1,STATUS)
          ENDIF
        ELSEIF (MODE.EQ.'TOG') THEN
          IF (I_MEM) THEN
            CALL IMG_TOGGLE(STATUS)
            CALL GCB_ATTACH('IMAGE',STATUS)
            CALL IMG_2DGCB(STATUS)
            I_DISP=.FALSE.
          ELSE
            CALL MSG_PRNT('AST_ERR: no image currently in cache')
          ENDIF
        ELSEIF (MODE.EQ.'CLE') THEN
          IF (I_MEM) THEN
            CALL IMG_DELCACHE(STATUS)
            I_MEM=.FALSE.
            IF (I_GUI) THEN
              CALL IMG_NBPUT0I('CACHE',0,STATUS)
            ENDIF
          ELSE
            CALL MSG_PRNT('AST_ERR: no image currently in cache')
          ENDIF
        ELSE

        ENDIF



      ENDIF

      CALL USI_CLOSE()

      END
