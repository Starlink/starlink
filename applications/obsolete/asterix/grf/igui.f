*+  IGUI - interface with GUI
      SUBROUTINE IGUI(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*        rjv@star.sr.bham.ac.uk
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'PRM_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*132 ACTION,VALUE
      INTEGER ID,SID,ITEMID
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IGUI Version 1.8-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      CALL USI_GET0C('ACTION',ACTION,STATUS)
      CALL CHR_UCASE(ACTION)

*  the GUI has just started up
      IF (ACTION.EQ.'START') THEN

*  get the name of the noticeboard that will pass information to GUI
        CALL USI_GET0C('VALUE',VALUE,STATUS)

*  create noticeboard
        CALL NBS_BEGIN_DEFINITION(ID,STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'X','_REAL',0,VAL__NBR,SID,STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'Y','_REAL',0,VAL__NBR,SID,STATUS)
        CALL NBS_END_DEFINITION(VALUE,'CREATE_NOTICEBOARD',STATUS)

*  initialise
        CALL NBS_FIND_NOTICEBOARD(VALUE,I_NBID,STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'X',ITEMID,STATUS)
        CALL NBS_PUT_VALUE(ITEMID,0,VAL__NBR,0.0,STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'Y',ITEMID,STATUS)
        CALL NBS_PUT_VALUE(ITEMID,0,VAL__NBR,0.0,STATUS)

        I_GUI=(STATUS.EQ.SAI__OK)

      ENDIF

      CALL USI_CLOSE()

      END
