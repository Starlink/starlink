*+  IZOOM - zoom image
      SUBROUTINE IZOOM(STATUS)
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
      REAL XC,YC,DX,DY
      REAL PX,PY
      INTEGER NX,NY
      INTEGER IPX,IPY
*    Global Variables :
      INCLUDE 'IMG_CMN'
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='IZOOM Version 1.8-0')
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

        ELSEIF (MODE.EQ.'OFF') THEN

          I_ZM_IX1=1
          I_ZM_IX2=I_NX
          I_ZM_IY1=1
          I_ZM_IY2=I_NY

        ENDIF


        CALL IMG_DISP(STATUS)

      ENDIF

      CALL USI_CLOSE()

      END
