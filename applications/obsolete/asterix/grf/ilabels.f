*+  ILABELS - change labels of current image or plot
      SUBROUTINE ILABELS(STATUS)
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
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*80 XLABEL,YLABEL
      INTEGER XCOL,YCOL
      INTEGER XOK,YOK
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ILABELS Version 1.7-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.(I_DISP.OR.I_DISP_1D)) THEN
        CALL MSG_PRNT('AST_ERR: no current image or plot displayed')
      ELSE

        CALL GCB_GETI('XLABEL_COLOUR',XOK,XCOL,STATUS)
        CALL GCB_GETI('YLABEL_COLOUR',YOK,YCOL,STATUS)

        CALL USI_GET0C('XLBL',XLABEL,STATUS)
        CALL USI_GET0C('YLBL',YLABEL,STATUS)

        CALL GCB_SETI('XLABEL_COLOUR',-1,STATUS)
        CALL GCB_SETI('YLABEL_COLOUR',-1,STATUS)

        CALL GCB_SETC('XLABEL_TEXT',XLABEL,STATUS)
        CALL GCB_SETC('YLABEL_TEXT',YLABEL,STATUS)

        IF (XOK) THEN
          CALL GCB_SETI('XLABEL_COLOUR',XCOL,STATUS)
        ELSE
          CALL GCB_CANI('XLABEL_COLOUR',STATUS)
        ENDIF
        IF (YOK) THEN
          CALL GCB_SETI('YLABEL_COLOUR',YCOL,STATUS)
        ELSE
          CALL GCB_CANI('YLABEL_COLOUR',STATUS)
        ENDIF


        CALL GFX_LABELS(' ',' ',STATUS)


      ENDIF

      CALL USI_CLOSE()

      END


