*+  ICOLOUR - change colour table
      SUBROUTINE ICOLOUR( STATUS )
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      17 Sep 90: V1.2-1 able to read external table (RJV)
*      19 Jan 93: V1.7-0 uses GCB and GFX
*      19 Mar 93: V1.7-1 NEG, RGB etc (RJV)
*      22 Mar 93: V1.7-2 GREY, RED etc (RJV)
*      29 Nov 93: V1.7-3 HELP and WRITE added (RJV)
*       8 Feb 94: V1.7-4 new format GCB (RJV)
*       5 May 94: V1.7-5 SMG converted to TSM (DJA)
*       2 Jun 94: V1.7-6 Bug fix (RJV)
*      16 Aug 94: V1.7-7 Bug fix (not reading colours) (RJV)
*      14 Aug 95: V1.8-0 Interface with GUI (RJV)
*      11 Jan 96: V1.8-1 Remove edit mode and TSM (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*10 MODE
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ICOLOUR Version 1.8-1')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSE

*  get mode
        MODE='HELP'
        DO WHILE (MODE.EQ.'HELP'.AND.STATUS.EQ.SAI__OK)
          CALL USI_GET0C('MODE',MODE,STATUS)
          CALL CHR_UCASE(MODE)
          IF (MODE.EQ.'HELP'.AND.STATUS.EQ.SAI__OK) THEN
            CALL ICOLOUR_MODEHELP()
            CALL USI_CANCL('MODE',STATUS)
          ELSEIF (STATUS.NE.SAI__OK) THEN
            MODE=' '
          ENDIF
        ENDDO

        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

        IF (MODE.EQ.'LOAD'.AND.I_GUI) THEN
          CALL ICOLOUR_GUI_LOAD(STATUS)
        ELSEIF (MODE.EQ.'UPDATE'.AND.I_GUI) THEN
          CALL ICOLOUR_GUI_UPDATE(STATUS)
        ELSEIF (MODE.EQ.'INVERT'.AND.I_GUI) THEN
          CALL ICOLOUR_GUI_INVERT(STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'NEGATIVE'.AND.I_GUI) THEN
          CALL ICOLOUR_GUI_NEGATIVE(STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'ROTATE'.AND.I_GUI) THEN
          CALL ICOLOUR_GUI_ROTATE(STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'SAVE'.AND.I_GUI) THEN
          CALL ICOLOUR_GUI_SAVE(STATUS)
        ELSEIF (MODE.EQ.'READ') THEN
          CALL ICOLOUR_RGB(.FALSE.,STATUS)
          CALL ICOLOUR_NEG(.FALSE.,STATUS)
          CALL ICOLOUR_READ(0,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'WRITE') THEN
          CALL ICOLOUR_WRITE(STATUS)
        ELSEIF (MODE.EQ.'NEG') THEN
          CALL ICOLOUR_NEG(.TRUE.,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'POS'.OR.MODE.EQ.'NONEG') THEN
          CALL ICOLOUR_NEG(.FALSE.,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'RGB') THEN
          CALL ICOLOUR_RGB(.TRUE.,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'NORGB') THEN
          CALL ICOLOUR_RGB(.FALSE.,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'NORM') THEN
          CALL ICOLOUR_RGB(.FALSE.,STATUS)
          CALL ICOLOUR_NEG(.FALSE.,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE(:3).EQ.'DEF') THEN
          CALL ICOLOUR_RGB(.FALSE.,STATUS)
          CALL ICOLOUR_NEG(.FALSE.,STATUS)
          CALL ICOLOUR_READ(1,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'GREY') THEN
          CALL ICOLOUR_RGB(.FALSE.,STATUS)
          CALL ICOLOUR_NEG(.FALSE.,STATUS)
          CALL ICOLOUR_READ(2,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'GREEN') THEN
          CALL ICOLOUR_RGB(.FALSE.,STATUS)
          CALL ICOLOUR_NEG(.FALSE.,STATUS)
          CALL ICOLOUR_READ(3,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'BLUE') THEN
          CALL ICOLOUR_RGB(.FALSE.,STATUS)
          CALL ICOLOUR_NEG(.FALSE.,STATUS)
          CALL ICOLOUR_READ(4,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'RED') THEN
          CALL ICOLOUR_RGB(.FALSE.,STATUS)
          CALL ICOLOUR_NEG(.FALSE.,STATUS)
          CALL ICOLOUR_READ(5,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE(:4).EQ.'RAIN') THEN
          CALL ICOLOUR_RGB(.FALSE.,STATUS)
          CALL ICOLOUR_NEG(.FALSE.,STATUS)
          CALL ICOLOUR_READ(6,STATUS)
          CALL GFX_SETCOLS(STATUS)


        ELSEIF (STATUS.EQ.SAI__OK) THEN
          CALL MSG_SETC('MOD',MODE)
          CALL MSG_PRNT('AST_ERR: invalid mode - ^MOD')
        ENDIF

        CALL GCB_CACHE(I_CACHE,STATUS)

      ENDIF

      CALL USI_CLOSE()

      END



*+  ICOLOUR_MODEHELP
      SUBROUTINE ICOLOUR_MODEHELP()
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
*    Import :
*    Function declarations :
*    Local constants :
      INTEGER NLINE
      PARAMETER (NLINE=6)
*    Local variables :
      CHARACTER*79 TEXT(NLINE)
      INTEGER I
*    Local data :
      DATA TEXT/
     :  '    DEFAULT  - default table     GREY    - grey scale',
     :  '    GREEN    - green table       BLUE    - blue/green table',
     :  '    RED      - red table         RAINBOW - rainbow colours',
     :  '    RGB      - 3-colour table    NEG     - invert colours',
     :  '    NORM     - reverse NEG       POS     - same as NORM',
     :  '    READ     - read from file    WRITE   - write to file'/
*-
      CALL MSG_BLNK()
      DO I=1,NLINE
        CALL MSG_PRNT(TEXT(I))
      ENDDO
      CALL MSG_BLNK()

      END


*+  ICOLOUR_READ
      SUBROUTINE ICOLOUR_READ(TAB,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Import :
      INTEGER TAB
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 REC
      CHARACTER*16 TABS(6)/'default.act','greyscale.act','green.act',
     :                     'bluegreen.act','red.act','rainbow.act'/
      CHARACTER FILE*132
      REAL COLTAB(3,16)
      INTEGER IFD
      INTEGER I
      INTEGER L
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (TAB.GE.1.AND.TAB.LE.6) THEN
          CALL PSX_GETENV('AST_ROOT',FILE,STATUS)
          L=CHR_LEN(FILE)
          FILE=FILE(:L)//'/data/grafix/'//TABS(TAB)
        ELSE
          CALL USI_GET0C('FILE',FILE,STATUS)
        ENDIF

        CALL FIO_OPEN(FILE,'READ','NONE',0,IFD,STATUS)

        I=1
        DO WHILE (I.LE.16.AND.STATUS.EQ.SAI__OK)
          CALL FIO_READF(IFD,REC,STATUS)
          READ(REC,*) COLTAB(1,I),COLTAB(2,I),COLTAB(3,I)
          I=I+1
        ENDDO

        CALL FIO_CLOSE(IFD,STATUS)

*  save it
        CALL GCB_SETL('COLOUR_RGB',.FALSE.,STATUS)
        CALL GCB_SETI('COLOUR_N',16,STATUS)
        DO I=1,16
          CALL GCB_SET1R('COLOUR_RED',I,1,COLTAB(1,I),STATUS)
          CALL GCB_SET1R('COLOUR_GREEN',I,1,COLTAB(2,I),STATUS)
          CALL GCB_SET1R('COLOUR_BLUE',I,1,COLTAB(3,I),STATUS)
        ENDDO


      ENDIF

      END


*+  ICOLOUR_WRITE
      SUBROUTINE ICOLOUR_WRITE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*132 FILE
      CHARACTER*16 REC
      REAL COLTAB(3,16)
      INTEGER IFD
      INTEGER I,N
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETI('COLOUR_N',OK,N,STATUS)
        IF (OK.AND.N.EQ.16) THEN
          DO I=1,N
            CALL GCB_GET1R('COLOUR_RED',I,1,OK,COLTAB(1,I),STATUS)
            CALL GCB_GET1R('COLOUR_GREEN',I,1,OK,COLTAB(2,I),STATUS)
            CALL GCB_GET1R('COLOUR_BLUE',I,1,OK,COLTAB(3,I),STATUS)
          ENDDO

          CALL USI_GET0C('FILE',FILE,STATUS)
          CALL FIO_OPEN(FILE,'WRITE','LIST',0,IFD,STATUS)
          DO I=1,16
            WRITE(REC,'(3(F4.2,1X))')
     :                       COLTAB(1,I),COLTAB(2,I),COLTAB(3,I)
            CALL FIO_WRITE(IFD,REC,STATUS)
          ENDDO
          CALL FIO_CLOSE(IFD,STATUS)

        ELSE
          CALL MSG_PRNT('AST_ERR: no colour table present')
        ENDIF

      ENDIF

      END


*+  ICOLOUR_NEG
      SUBROUTINE ICOLOUR_NEG(NEG,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
*    Import :
      LOGICAL NEG
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_SETL('COLOUR_NEG',NEG,STATUS)

      ENDIF

      END

*+  ICOLOUR_RGB
      SUBROUTINE ICOLOUR_RGB(RGB,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
*    Import :
      LOGICAL RGB
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_SETL('COLOUR_RGB',RGB,STATUS)

      ENDIF

      END




*+  ICOLOUR_GUI_LOAD
      SUBROUTINE ICOLOUR_GUI_LOAD(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Global variables :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Global variables :
      INCLUDE 'IMG_CMN'
      REAL COL(3,16)
      INTEGER NCOL,NSHADE,FIRST,LAST
      COMMON /ICOLOUR_GUI_CMN/ COL,NCOL,NSHADE,FIRST,LAST
*    Local variables :
      CHARACTER*8 NAME
      INTEGER BGCOL
      INTEGER I,N
      INTEGER CID,ID
      INTEGER ICOL
      INTEGER NB
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN


*  get current colour table from GCB
        CALL GCB_GETI('COLOUR_N',OK,N,STATUS)
        IF (OK.AND.N.EQ.16) THEN
          DO I=1,N
            CALL GCB_GET1R('COLOUR_RED',I,1,OK,COL(1,I),STATUS)
            CALL GCB_GET1R('COLOUR_GREEN',I,1,OK,COL(2,I),STATUS)
            CALL GCB_GET1R('COLOUR_BLUE',I,1,OK,COL(3,I),STATUS)
          ENDDO
        ELSE
          CALL GFX_DEFCOLS(COL,STATUS)
        ENDIF

*  get colour capability of device
        CALL GDV_COLOURS(BGCOL,FIRST,LAST,STATUS)
*  how many useable colours
        NCOL=LAST-FIRST+1

*  how many shades between main colours
        NSHADE=(NCOL-16)/15

*  copy colour table to noticeboard
        DO I=1,9
          NAME='RED'
          WRITE(NAME(4:4),'(I1)') I
          CALL NBS_FIND_ITEM(I_NBID,NAME,ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,COL(1,I),STATUS)
          NAME='GREEN'
          WRITE(NAME(6:6),'(I1)') I
          CALL NBS_FIND_ITEM(I_NBID,NAME,ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,COL(2,I),STATUS)
          NAME='BLUE'
          WRITE(NAME(5:5),'(I1)') I
          CALL NBS_FIND_ITEM(I_NBID,NAME,ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,COL(3,I),STATUS)
        ENDDO
        DO I=10,16
          NAME='RED'
          WRITE(NAME(4:5),'(I2)') I
          CALL NBS_FIND_ITEM(I_NBID,NAME,ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,COL(1,I),STATUS)
          NAME='GREEN'
          WRITE(NAME(6:7),'(I2)') I
          CALL NBS_FIND_ITEM(I_NBID,NAME,ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,COL(2,I),STATUS)
          NAME='BLUE'
          WRITE(NAME(5:6),'(I2)') I
          CALL NBS_FIND_ITEM(I_NBID,NAME,ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,COL(3,I),STATUS)
        ENDDO


      ENDIF


      END


*+  ICOLOUR_GUI_UPDATE
      SUBROUTINE ICOLOUR_GUI_UPDATE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Global variables :
      INCLUDE 'IMG_CMN'
      REAL COL(3,16)
      INTEGER NCOL,NSHADE,FIRST,LAST
      COMMON /ICOLOUR_GUI_CMN/ COL,NCOL,NSHADE,FIRST,LAST
*    Local variables :
      CHARACTER*8 RNAME,GNAME,BNAME
      REAL RED,GREEN,BLUE
      REAL REDHUE,GREENHUE,BLUEHUE
      INTEGER FLAG
      INTEGER CID,RID,GID,BID,FID
      INTEGER NB
      INTEGER CI,CJ
      INTEGER J,K,J1,J2
      INTEGER ICOL,JCOL
*-
      IF (STATUS.EQ.SAI__OK.AND.NCOL.GE.16) THEN

        CALL NBS_FIND_ITEM(I_NBID,'COLOUR',CID,STATUS)
        CALL NBS_FIND_ITEM(I_NBID,'FLAG',FID,STATUS)

*  which is initial colour being changed
        CALL NBS_GET_VALUE(CID,0,VAL__NBI,ICOL,NB,STATUS)

*  monitor noticeboard until flag goes non-zero
        FLAG=0
        DO WHILE (FLAG.EQ.0)

*  convert to colour index within full range
          J1=MAX(1,ICOL-1)
          J2=MIN(16,ICOL+1)
          CI=FIRST+(J1-1)*(NSHADE+1)

*  get new primary colours
          RNAME='RED'
          GNAME='GREEN'
          BNAME='BLUE'
          IF (ICOL.LT.10) THEN
            WRITE(RNAME(4:4),'(I1)') ICOL
            WRITE(GNAME(6:6),'(I1)') ICOL
            WRITE(BNAME(5:5),'(I1)') ICOL
          ELSE
            WRITE(RNAME(4:5),'(I2)') ICOL
            WRITE(GNAME(6:7),'(I2)') ICOL
            WRITE(BNAME(5:6),'(I2)') ICOL
          ENDIF
          CALL NBS_FIND_ITEM(I_NBID,RNAME,RID,STATUS)
          CALL NBS_FIND_ITEM(I_NBID,GNAME,GID,STATUS)
          CALL NBS_FIND_ITEM(I_NBID,BNAME,BID,STATUS)

          JCOL=ICOL
          DO WHILE (ICOL.EQ.JCOL.AND.FLAG.EQ.0)

            CJ=CI

*  get current colours from noticeboard
            CALL NBS_GET_VALUE(RID,0,VAL__NBR,RED,NB,STATUS)
            COL(1,ICOL)=RED
            CALL NBS_GET_VALUE(GID,0,VAL__NBR,GREEN,NB,STATUS)
            COL(2,ICOL)=GREEN
            CALL NBS_GET_VALUE(BID,0,VAL__NBR,BLUE,NB,STATUS)
            COL(3,ICOL)=BLUE

*  set colour
            DO J=J1,J2-1
              RED=COL(1,J)
              REDHUE=(COL(1,J+1)-RED)/REAL(NSHADE+1)
              GREEN=COL(2,J)
              GREENHUE=(COL(2,J+1)-GREEN)/REAL(NSHADE+1)
              BLUE=COL(3,J)
              BLUEHUE=(COL(3,J+1)-BLUE)/REAL(NSHADE+1)
              CALL PGSCR(CJ,RED,GREEN,BLUE)
              DO K=1,NSHADE
                CJ=CJ+1
                RED=RED+REDHUE
                GREEN=GREEN+GREENHUE
                BLUE=BLUE+BLUEHUE
                CALL PGSCR(CJ,RED,GREEN,BLUE)
              ENDDO
              CJ=CJ+1
            ENDDO
            RED=COL(1,J2)
            GREEN=COL(2,J2)
            BLUE=COL(3,J2)
            CALL PGSCR(CJ,RED,GREEN,BLUE)

*  see if colour has changed
            CALL NBS_GET_VALUE(CID,0,VAL__NBI,ICOL,NB,STATUS)
*  see if GUI is still sending data
            CALL NBS_GET_VALUE(FID,0,VAL__NBI,FLAG,NB,STATUS)

          ENDDO

        ENDDO

      ENDIF

      END


*+  ICOLOUR_GUI_NEGATIVE
      SUBROUTINE ICOLOUR_GUI_NEGATIVE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Global variables :
      INCLUDE 'IMG_CMN'
      REAL COL(3,16)
      INTEGER NCOL,NSHADE,FIRST,LAST
      COMMON /ICOLOUR_GUI_CMN/ COL,NCOL,NSHADE,FIRST,LAST
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK.AND.NCOL.GE.16) THEN

        DO I=1,3
          DO J=1,16
            COL(I,J)=1.0-COL(I,J)
          ENDDO
        ENDDO

        DO J=1,16
          CALL GCB_SET1R('COLOUR_RED',J,1,COL(1,J),STATUS)
          CALL GCB_SET1R('COLOUR_GREEN',J,1,COL(2,J),STATUS)
          CALL GCB_SET1R('COLOUR_BLUE',J,1,COL(3,J),STATUS)
        ENDDO

      ENDIF

      END



*+  ICOLOUR_GUI_INVERT
      SUBROUTINE ICOLOUR_GUI_INVERT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Global variables :
      INCLUDE 'IMG_CMN'
      REAL COL(3,16)
      INTEGER NCOL,NSHADE,FIRST,LAST
      COMMON /ICOLOUR_GUI_CMN/ COL,NCOL,NSHADE,FIRST,LAST
*    Local variables :
      REAL COL2(3,16)
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK.AND.NCOL.GE.16) THEN

        DO I=1,16
          DO J=1,3
            COL2(J,I)=COL(J,17-I)
          ENDDO
        ENDDO

        DO I=1,16
          DO J=1,3
            COL(J,I)=COL2(J,I)
          ENDDO
        ENDDO

        DO J=1,16
          CALL GCB_SET1R('COLOUR_RED',J,1,COL(1,J),STATUS)
          CALL GCB_SET1R('COLOUR_GREEN',J,1,COL(2,J),STATUS)
          CALL GCB_SET1R('COLOUR_BLUE',J,1,COL(3,J),STATUS)
        ENDDO

      ENDIF

      END


*+  ICOLOUR_GUI_ROTATE
      SUBROUTINE ICOLOUR_GUI_ROTATE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Global variables :
      INCLUDE 'IMG_CMN'
      REAL COL(3,16)
      INTEGER NCOL,NSHADE,FIRST,LAST
      COMMON /ICOLOUR_GUI_CMN/ COL,NCOL,NSHADE,FIRST,LAST
*    Local variables :
      REAL RED,GREEN,BLUE
      INTEGER I
*-
      IF (STATUS.EQ.SAI__OK.AND.NCOL.GE.16) THEN

        DO I=1,16
          RED=COL(1,I)
          GREEN=COL(2,I)
          BLUE=COL(3,I)
          COL(2,I)=RED
          COL(3,I)=GREEN
          COL(1,I)=BLUE
          CALL GCB_SET1R('COLOUR_RED',I,1,COL(1,I),STATUS)
          CALL GCB_SET1R('COLOUR_GREEN',I,1,COL(2,I),STATUS)
          CALL GCB_SET1R('COLOUR_BLUE',I,1,COL(3,I),STATUS)
        ENDDO


      ENDIF

      END


*+  ICOLOUR_GUI_SAVE
      SUBROUTINE ICOLOUR_GUI_SAVE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Global variables :
      REAL COL(3,16)
      INTEGER NCOL,NSHADE,FIRST,LAST
      COMMON /ICOLOUR_GUI_CMN/ COL,NCOL,NSHADE,FIRST,LAST
*    Local variables :
      INTEGER I
*-

      IF (STATUS.EQ.SAI__OK) THEN


*  get current colour table
        CALL GCB_SETI('COLOUR_N',16,STATUS)
        DO I=1,16
          CALL GCB_SET1R('COLOUR_RED',I,1,COL(1,I),STATUS)
          CALL GCB_SET1R('COLOUR_GREEN',I,1,COL(2,I),STATUS)
          CALL GCB_SET1R('COLOUR_BLUE',I,1,COL(3,I),STATUS)
        ENDDO


      ENDIF

      END
