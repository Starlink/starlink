*+  IZOOM - zoom image
      SUBROUTINE IZOOM(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*      19 Apr 96 : V2.0-0 zoom to current region option (RJV)
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
      REAL XC,YC,DX,DY
      REAL PX,PY
      REAL FACTOR
      INTEGER NX,NY
      INTEGER IPX,IPY
*    Global Variables :
      INCLUDE 'IMG_CMN'
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='IZOOM Version 2.2-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)


      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  ensure transformations are correct
        CALL GTR_RESTORE(STATUS)

        CALL USI_GET0C('MODE',MODE,STATUS)
        CALL CHR_UCASE(MODE)
        MODE=MODE(:3)

        IF (MODE.EQ.'BOX') THEN
          CALL IMG_GETBOX('XCENT','YCENT','XWID','YWID',XC,YC,DX,DY,
     :                                                        STATUS)
          CALL IMG_BOX(XC,YC,DX,DY,STATUS)
          CALL IMG_BOXTOBOX(XC,YC,DX,DY,I_ZM_IX1,I_ZM_IX2,
     :                            I_ZM_IY1,I_ZM_IY2,STATUS)

        ELSEIF (MODE.EQ.'FAC') THEN

          CALL USI_GET0R('FACTOR',FACTOR,STATUS)
          NX=INT(REAL(I_NX)/FACTOR)
          NY=INT(REAL(I_NY)/FACTOR)
          CALL IMG_WORLDTOPIX(I_X,I_Y,PX,PY,STATUS)
          IPX=INT(PX+0.5)
          IPY=INT(PY+0.5)
          I_ZM_IX1=MAX(IPX-NX/2,1)
          I_ZM_IX2=MIN(IPX+NX/2,I_NX)
          I_ZM_IY1=MAX(IPY-NY/2,1)
          I_ZM_IY2=MIN(IPY+NY/2,I_NY)

        ELSEIF (MODE.EQ.'REG') THEN
          I_ZM_IX1=I_IX1
          I_ZM_IX2=I_IX2
          I_ZM_IY1=I_IY1
          I_ZM_IY2=I_IY2

        ELSEIF (MODE.EQ.'OFF') THEN

          I_ZM_IX1=1
          I_ZM_IX2=I_NX
          I_ZM_IY1=1
          I_ZM_IY2=I_NY

        ENDIF

*  clear display
        IF (I_SPLIT_DISP) THEN
          CALL IMG_CLEAR(1,STATUS)
        ELSE
          CALL GDV_CLEAR(STATUS)
        ENDIF

*  reset transformation database
        CALL GTR_ZERO(STATUS)

        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

*  display image
        CALL IMG_WINDOW(STATUS)
        CALL IMG_DISP(STATUS)
        CALL IMG_AXES(STATUS)

*  flag current plotting status
        CALL GCB_SETL('SURF_FLAG',.FALSE.,STATUS)
        I_DISP=.TRUE.
         I_DISP_1D=.FALSE.

      ENDIF

      CALL USI_CLOSE()

      END
