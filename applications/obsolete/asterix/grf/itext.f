*+  ITEXT - put text on image or plot at arbitrary position
      SUBROUTINE ITEXT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*          19 Feb 92: V1.2-1 improvement to interactive mode
*           1 Feb 93: V1.7-0 records each entry using GCB (RJV)
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
      CHARACTER*80 TEXT
      CHARACTER*1 CH
      INTEGER N
      REAL XC,YC,XE,YE
      REAL ANGLE
      LOGICAL OK
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ITEXT Version 1.7-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSE


*  cursor mode
        IF (I_MODE.EQ.1) THEN

          CALL MSG_BLNK()
          XC=I_X
          YC=I_Y
          CALL MSG_SETR('X',XC)
          CALL MSG_SETR('Y',YC)
          CALL MSG_PRNT('Select starting point/^X,^Y/...')
          CALL PGCURSE(XC,YC,CH)
          IF (CH.EQ.CHAR(13).OR.CH.EQ.CHAR(50)) THEN
            XC=I_X
            YC=I_Y
          ENDIF
          CALL MSG_PRNT('Select orientation...')
          XE=XC
          YE=YC
          CALL PGCURSE(XE,YE,CH)
          IF (CH.EQ.CHAR(13).OR.CH.EQ.CHAR(50)) THEN
            ANGLE=0.0
          ELSE
            ANGLE=ATAN2(YE-YC,XE-XC)*180.0/3.142
          ENDIF
          CALL USI_GET0C('TEXT',TEXT,STATUS)

*  keyboard mode
        ELSE

          CALL USI_GET0C('TEXT',TEXT,STATUS)
          CALL USI_GET0R('XPOS',XC,STATUS)
          CALL USI_GET0R('YPOS',YC,STATUS)
          CALL USI_GET0R('ANGLE',ANGLE,STATUS)

        ENDIF

        CALL PGPTEXT(XC,YC,ANGLE,0.0,TEXT)

*  record in GCB
        CALL GCB_GETI('NOTE_N',OK,N,STATUS)
        IF (.NOT.OK) THEN
          N=0
        ENDIF
        N=N+1
        CALL GCB_SETI('NOTE_N',N,STATUS)
        CALL GCB_SET1C('NOTE_TEXT',N,1,TEXT,STATUS)
        CALL GCB_SET1R('NOTE_X',N,1,XC,STATUS)
        CALL GCB_SET1R('NOTE_Y',N,1,YC,STATUS)
        CALL GCB_SET1R('NOTE_ANGLE',N,1,ANGLE,STATUS)

      ENDIF

      CALL USI_CLOSE()

      END


