*+  ITITLE - change title of current image or plot
      SUBROUTINE ITITLE(STATUS)
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
      CHARACTER*80 TITLE
      INTEGER COL
      INTEGER N
      LOGICAL OK
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ITITLE Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.(I_DISP.OR.I_DISP_1D)) THEN
        CALL MSG_PRNT('AST_ERR: no current image or plot displayed')
      ELSE

        CALL GCB_GETI('TITLE_N',OK,N,STATUS)
        IF (.NOT.OK) THEN
          N=0
        ENDIF
        IF (N.GT.1) THEN
          CALL MSG_PRNT(
     :          'AST_ERR: can only use this for single title lines')
          CALL MSG_SETI('N',N)
          CALL MSG_PRNT(
     :          '       - there are ^N lines defined')
          STATUS=SAI__ERROR
        ENDIF

        CALL PAR_GET0C('TITLE',TITLE,STATUS)


        CALL GCB_GET1I('TITLE_COLOUR',1,1,OK,COL,STATUS)
        CALL GCB_SET1I('TITLE_COLOUR',1,1,-1,STATUS)
        CALL GFX_TITLE(' ',STATUS)
        IF (OK) THEN
          CALL GCB_SET1I('TITLE_COLOUR',1,1,COL,STATUS)
        ELSE
          CALL GCB_CAN1I('TITLE_COLOUR',1,1,STATUS)
        ENDIF
        CALL GCB_SET1C('TITLE_TEXT',1,1,TITLE,STATUS)
        CALL GCB_SETI('TITLE_N',1,STATUS)
        CALL GFX_TITLE(' ',STATUS)


      ENDIF

      END
