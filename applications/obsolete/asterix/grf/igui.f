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
      INTEGER ID,SID,ITEMID,GCBID
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IGUI Version 1.8-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      CALL USI_GET0C('ACTION',ACTION,STATUS)
      CALL CHR_UCASE(ACTION)

      IF (ACTION.EQ.'INIT') THEN

        CALL USI_GET0C('VALUE',VALUE,STATUS)
        CALL CHR_UCASE(VALUE)

        IF (VALUE.EQ.'GCB') THEN

*  initialise GCB
          CALL GCB_RDDSCF(STATUS)

        ENDIF

      ELSEIF (ACTION.EQ.'CREATE') THEN

*  get the name of the noticeboard that will pass information to GUI
        CALL USI_GET0C('VALUE',VALUE,STATUS)

*  create noticeboard
        CALL IGUI_CRENB(VALUE,STATUS)

        I_GUI=(STATUS.EQ.SAI__OK)

      ELSEIF (ACTION.EQ.'DESTROY') THEN

*  destroy noticeboard
        CALL NBS_LOSE_NOTICEBOARD(I_NBID,'FORCE',STATUS)
        I_GUI=.FALSE.

*  update noticeboard with current GCB
      ELSEIF (ACTION.EQ.'UPDATE') THEN

        CALL NBS_FIND_ITEM(I_NBID,'GCB',GCBID,STATUS)
        CALL GCB_FILLSHADOW(GCBID,STATUS)

*  update noticeboard with current coord transformation
      ELSEIF (ACTION.EQ.'TRANSFORM') THEN

        CALL IGUI_TRANSFORM(STATUS)

      ENDIF

      CALL USI_CLOSE()

      END


*+  IGUI_CRENB - create noticeboard
      SUBROUTINE IGUI_CRENB(NAME,STATUS)
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
*    Import :
      CHARACTER*(*) NAME
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ID,SID,ITEMID
      INTEGER GCBID
      INTEGER I
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL NBS_BEGIN_DEFINITION(ID,STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'X','_REAL',0,VAL__NBR,SID,STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'Y','_REAL',0,VAL__NBR,SID,STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'RA','_CHAR',0,12,SID,STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'DEC','_CHAR',0,12,SID,STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'MIN','_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'MAX','_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'XMIN','_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'XMAX','_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'YMIN','_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
        CALL NBS_DEFINE_PRIMITIVE(ID,'YMAX','_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
        DO I=1,9
          NAME='RED'
          WRITE(NAME(4:4),'(I1)') I
          CALL NBS_DEFINE_PRIMITIVE(ID,NAME,'_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
          NAME='GREEN'
          WRITE(NAME(6:6),'(I1)') I
          CALL NBS_DEFINE_PRIMITIVE(ID,NAME,'_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
          NAME='BLUE'
          WRITE(NAME(5:5),'(I1)') I
          CALL NBS_DEFINE_PRIMITIVE(ID,NAME,'_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
        ENDDO
        DO I=10,16
          NAME='RED'
          WRITE(NAME(4:5),'(I2)') I
          CALL NBS_DEFINE_PRIMITIVE(ID,NAME,'_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
          NAME='GREEN'
          WRITE(NAME(6:7),'(I2)') I
          CALL NBS_DEFINE_PRIMITIVE(ID,NAME,'_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
          NAME='BLUE'
          WRITE(NAME(5:6),'(I2)') I
          CALL NBS_DEFINE_PRIMITIVE(ID,NAME,'_REAL',0,VAL__NBR,SID,
     :                                                       STATUS)
        ENDDO
        CALL NBS_DEFINE_PRIMITIVE(ID,'COLOUR','_INTEGER',0,VAL__NBI,
     :                                                    SID,STATUS)

*  create shadow of GCB
        CALL NBS_DEFINE_STRUCTURE(ID,'GCB','GCB',GCBID,STATUS)
        CALL GCB_CRESHADOW(GCBID,STATUS)

        CALL NBS_END_DEFINITION(NAME,'CREATE_NOTICEBOARD',STATUS)

*  initialise
        CALL NBS_FIND_NOTICEBOARD(NAME,I_NBID,STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'X',ITEMID,STATUS)
        CALL NBS_PUT_VALUE(ITEMID,0,VAL__NBR,0.0,STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'Y',ITEMID,STATUS)
        CALL NBS_PUT_VALUE(ITEMID,0,VAL__NBR,0.0,STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'RA',ITEMID,STATUS)
        CALL NBS_PUT_CVALUE(ITEMID,0,' ',STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'DEC',ITEMID,STATUS)
        CALL NBS_PUT_CVALUE(ITEMID,0,' ',STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'MIN',ITEMID,STATUS)
        CALL NBS_PUT_VALUE(ITEMID,0,VAL__NBR,0.0,STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'MAX',ITEMID,STATUS)
        CALL NBS_PUT_VALUE(ITEMID,0,VAL__NBR,0.0,STATUS)

      ENDIF

      END




*+  IGUI_TRANSFORM
      SUBROUTINE IGUI_TRANSFORM(STATUS)
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
*    Import :
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ITEMID
      REAL XMIN,XMAX,YMIN,YMAX
*-
      IF (STATUS.EQ.SAI__OK) THEN

        XMIN=I_XBASE-0.5*I_XSCALE
        XMAX=XMIN+REAL(I_NX)*I_XSCALE
        YMIN=I_YBASE-0.5*I_YSCALE
        YMAX=YMIN+REAL(I_NY)*I_YSCALE
        CALL NBS_FIND_ITEM(I_NBID,'XMIN',ITEMID,STATUS)
        CALL NBS_PUT_VALUE(ITEMID,0,VAL__NBR,XMIN,STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'XMAX',ITEMID,STATUS)
        CALL NBS_PUT_VALUE(ITEMID,0,VAL__NBR,XMAX,STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'YMIN',ITEMID,STATUS)
        CALL NBS_PUT_VALUE(ITEMID,0,VAL__NBR,YMIN,STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'YMAX',ITEMID,STATUS)
        CALL NBS_PUT_VALUE(ITEMID,0,VAL__NBR,YMAX,STATUS)

      ENDIF

      END
