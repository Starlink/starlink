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
      CHARACTER*10 MODE
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ICOLOUR Version 1.7-7')
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

        IF (MODE.EQ.'READ') THEN
          CALL ICOLOUR_RGB(.FALSE.,STATUS)
          CALL ICOLOUR_NEG(.FALSE.,STATUS)
          CALL ICOLOUR_READ(0,STATUS)
          CALL GFX_SETCOLS(STATUS)
        ELSEIF (MODE.EQ.'WRITE') THEN
          CALL ICOLOUR_WRITE(STATUS)
        ELSEIF (MODE.EQ.'EDIT') THEN
          CALL ICOLOUR_EDIT(STATUS)
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Import :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      INTEGER NLINE
      PARAMETER (NLINE=8)
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
     :  '    READ     - read table from HDS file/object',
     :  '    WRITE    - write current table to HDS file/object',
     :  '    EDIT     - edit current table'/
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
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Import :
      INTEGER TAB
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(DAT__SZTYP) TYPE
      CHARACTER TABLE*10,FILE*132
      REAL COLTAB(3,16)
      INTEGER NVAL,NCHAR
      INTEGER I
      INTEGER TABNO
      INTEGER DIMS(2)
      LOGICAL OK,NUMBER
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DIMS(1)=3
        DIMS(2)=16

        NUMBER=.FALSE.
        IF (TAB.EQ.0) THEN
          CALL USI_DASSOC('TABLE','READ',LOC,STATUS)
          CALL DAT_TYPE(LOC,TYPE,STATUS)
          CALL DAT_SIZE(LOC,NVAL,STATUS)
          OK=.TRUE.
          IF (TYPE.EQ.'_INTEGER'.AND.NVAL.EQ.1) THEN
            CALL DAT_GET0I(LOC,TABNO,STATUS)
            CALL DAT_ANNUL(LOC,STATUS)
            NUMBER=.TRUE.
          ENDIF
        ELSE
          TABNO=TAB
          NUMBER=.TRUE.
          OK=.TRUE.
        ENDIF

        IF (NUMBER) THEN
          CALL CHR_ITOC(TABNO,TABLE,NCHAR)
          TABLE='AST_TAB'//TABLE(:NCHAR)
          CALL PSX_GETENV(TABLE,FILE,STATUS)
          CALL HDS_OPEN(FILE,'READ',LOC,STATUS)
          IF (STATUS.NE.SAI__OK) THEN
            CALL MSG_PRNT('AST_ERR: unknown table number')
            OK=.FALSE.
          ENDIF

*  check type of external table
        ELSEIF (TYPE.NE.'_REAL') THEN
          CALL MSG_PRNT('AST_ERR: invalid data type in table')
          OK=.FALSE.
*  check number of entries
        ELSE
          IF (NVAL.NE.48) THEN
            CALL MSG_PRNT('AST_ERR: incorrect number of entries'
     :                                          //' in table')
            OK=.FALSE.
          ENDIF
        ENDIF

*  read table
        IF (OK.AND.STATUS.EQ.SAI__OK) THEN

          CALL DAT_GETR(LOC,2,DIMS,COLTAB,STATUS)

*  save it
          CALL GCB_SETL('COLOUR_RGB',.FALSE.,STATUS)
          CALL GCB_SETI('COLOUR_N',16,STATUS)
          DO I=1,16
            CALL GCB_SET1R('COLOUR_RED',I,1,COLTAB(1,I),STATUS)
            CALL GCB_SET1R('COLOUR_GREEN',I,1,COLTAB(2,I),STATUS)
            CALL GCB_SET1R('COLOUR_BLUE',I,1,COLTAB(3,I),STATUS)
          ENDDO

        ENDIF

        CALL DAT_ANNUL(LOC,STATUS)

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
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Import :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      REAL COLTAB(3,16)
      INTEGER DIMS(2)
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
          DIMS(1)=3
          DIMS(2)=16
          CALL USI_DCREAT('TABLE','_REAL',2,DIMS,STATUS)
          CALL USI_DASSOC('TABLE','WRITE',LOC,STATUS)
          CALL DAT_PUTR(LOC,2,DIMS,COLTAB,STATUS)
          CALL DAT_ANNUL(LOC,STATUS)
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
      INCLUDE 'DAT_PAR'
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
      INCLUDE 'DAT_PAR'
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



*+  ICOLOUR_EDIT
      SUBROUTINE ICOLOUR_EDIT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
      STRUCTURE /SMG/
        INTEGER TITLE
        INTEGER MAIN
        INTEGER HELP
      ENDSTRUCTURE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      RECORD /SMG/ SCREEN
      REAL COLTAB(3,16)
      INTEGER I,N,NCOLS,NROWS
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  set up screen managment stuff
        CALL TSM_INIT( ' ', STATUS )
        CALL TSM_GETDIMS( 0, NCOLS, NROWS, STATUS )
        CALL TSM_CREWIN( 80, 2, 1, 1, SCREEN.TITLE, STATUS )
        CALL TSM_CREWINB( 44, 18, 2, 4, SCREEN.MAIN, STATUS )
        CALL TSM_CREWINB( 29, 18, 49, 4, SCREEN.HELP, STATUS )

*  get current colour table
        CALL GCB_GETI('COLOUR_N',OK,N,STATUS)
        IF (OK.AND.N.EQ.16) THEN
          DO I=1,N
            CALL GCB_GET1R('COLOUR_RED',I,1,OK,COLTAB(1,I),STATUS)
            CALL GCB_GET1R('COLOUR_GREEN',I,1,OK,COLTAB(2,I),STATUS)
            CALL GCB_GET1R('COLOUR_BLUE',I,1,OK,COLTAB(3,I),STATUS)
          ENDDO
        ELSE
          CALL GFX_DEFCOLS(COLTAB,STATUS)
        ENDIF

*  display screen
        CALL ICOLOUR_SCREEN(SCREEN,COLTAB,STATUS)

*  interactive bit
        CALL ICOLOUR_SETCOL(SCREEN,COLTAB,STATUS)

*  save it
        CALL GCB_SETL('COLOUR_RGB',.FALSE.,STATUS)
        CALL GCB_SETI('COLOUR_N',16,STATUS)
        DO I=1,16
          CALL GCB_SET1R('COLOUR_RED',I,1,COLTAB(1,I),STATUS)
          CALL GCB_SET1R('COLOUR_GREEN',I,1,COLTAB(2,I),STATUS)
          CALL GCB_SET1R('COLOUR_BLUE',I,1,COLTAB(3,I),STATUS)
        ENDDO

*  close down screen management
        CALL TSM_DELWIN( SCREEN.HELP, STATUS )
        CALL TSM_DELWIN( SCREEN.MAIN, STATUS )
        CALL TSM_DELWIN( SCREEN.TITLE, STATUS )

        CALL ERR_MARK( STATUS )
        CALL TSM_CLOSE( STATUS )
        CALL ERR_RLSE( STATUS )

      ENDIF

      END



*+
      SUBROUTINE ICOLOUR_SCREEN(SCREEN,COL,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
      STRUCTURE /SMG/
        INTEGER TITLE
        INTEGER MAIN
        INTEGER HELP
      ENDSTRUCTURE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'TSM_PAR'
*    Import :
*    Import-export :
      RECORD /SMG/ SCREEN
      REAL COL(3,16)
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      INTEGER ICOL
      INTEGER ICOMP
      CHARACTER*10 COLSTR
*-

*  titles
      CALL TSM_PUTLABEL( SCREEN.MAIN, 'COLOUR  TABLE', TSM__TOP,
     :                   TSM__CENTRE, STATUS )
      CALL TSM_PUTLABEL( SCREEN.HELP, 'KEYS', TSM__TOP,
     :                   TSM__CENTRE, STATUS )

*  column headings
      CALL TSM_PUTSTRAT( SCREEN.MAIN, '  Colour #', 1, 1, STATUS )
      CALL TSM_PUTSTRAT( SCREEN.MAIN, '  Red', 13, 1, STATUS )
      CALL TSM_PUTSTRAT( SCREEN.MAIN, '  Green', 23, 1, STATUS )
      CALL TSM_PUTSTRAT( SCREEN.MAIN, '  Blue', 33, 1, STATUS )

*  put on table values
      DO ICOL=1,16
        WRITE(COLSTR,'(I5,5X)') ICOL
        CALL TSM_PUTSTRAT( SCREEN.MAIN, colstr, 1, ICOL+2, STATUS )
        DO ICOMP=1,3
          CALL ICOLOUR_SHOWVAL(SCREEN.MAIN,ICOL,ICOMP,COL,STATUS)
        ENDDO
      ENDDO

*  fill help screen
      CALL ICOLOUR_HELP(SCREEN.HELP,STATUS)

*  display screens
      CALL TSM_REFRESH( SCREEN.TITLE, STATUS )
      CALL TSM_REFRESH( SCREEN.HELP, STATUS )
      CALL TSM_REFRESH( SCREEN.MAIN, STATUS )

      END



*+
      SUBROUTINE ICOLOUR_SETCOL(SCREEN,COL,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
      STRUCTURE /SMG/
        INTEGER TITLE
        INTEGER MAIN
        INTEGER HELP
      ENDSTRUCTURE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'TSM_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      RECORD /SMG/ SCREEN
*    Import-export :
      REAL COL(3,16)
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      REAL INC
      PARAMETER (INC=0.02)
*    Local variables :
      REAL RED,GREEN,BLUE
      REAL REDHUE,GREENHUE,BLUEHUE
      INTEGER NCOL,NSHADE
      INTEGER BGCOL,FIRST,LAST
      INTEGER CI
      INTEGER J,K,J1,J2
      INTEGER ICHR
      INTEGER ICOL
      INTEGER ICOMP
      LOGICAL CHANGE
*    Global variables :
*-
      IF (STATUS.NE.SAI__OK) RETURN

*  get colour capability of device
      CALL GDV_COLOURS(BGCOL,FIRST,LAST,STATUS)
*  how many useable colours
      NCOL=LAST-FIRST+1

      IF (NCOL.LT.8) THEN
        CALL MSG_PRNT('AST_ERR: device has insufficient colours')
        STATUS=SAI__ERROR
        GOTO 999
      ENDIF

*  setup start values
      ICOL=1
      ICOMP=1
      CALL ICOLOUR_HIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
      ICHR=0

      DO WHILE (ICHR.NE.TSM__K_LOWERCASE_Q.AND.
     :          ICHR.NE.TSM__K_UPPERCASE_Q)
        CHANGE=.FALSE.

        CALL TSM_RDCH( SCREEN.MAIN, ICHR, STATUS )

*  different logical colour
        IF (ICHR.EQ.TSM__K_LEFT) THEN
          CALL ICOLOUR_UNHIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
          ICOMP=ICOMP-1
          IF (ICOMP.EQ.0) THEN
            ICOMP=3
          ENDIF
          CALL ICOLOUR_HIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
        ELSEIF (ICHR.EQ.TSM__K_RIGHT) THEN
          CALL ICOLOUR_UNHIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
          ICOMP=ICOMP+1
          IF (ICOMP.GT.3) THEN
            ICOMP=1
          ENDIF
          CALL ICOLOUR_HIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)

*  different primary colour
        ELSEIF (ICHR.EQ.TSM__K_UP) THEN
          CALL ICOLOUR_UNHIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
          ICOL=ICOL-1
          IF (ICOL.LT.1) THEN
            ICOL=16
          ENDIF
          CALL ICOLOUR_HIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
        ELSEIF (ICHR.EQ.TSM__K_DOWN) THEN
          CALL ICOLOUR_UNHIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
          ICOL=ICOL+1
          IF (ICOL.GT.16) THEN
            ICOL=1
          ENDIF
          CALL ICOLOUR_HIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)

*  change intensity
        ELSEIF (ICHR.EQ.TSM__K_GREATER_THAN) THEN
          COL(ICOMP,ICOL)=COL(ICOMP,ICOL)+INC
          IF (COL(ICOMP,ICOL).GT.1.0) THEN
            COL(ICOMP,ICOL)=1.0
          ENDIF
          CALL ICOLOUR_SHOWVAL(SCREEN.MAIN,ICOL,ICOMP,COL,STATUS)
          CALL ICOLOUR_HIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
          CHANGE=.TRUE.
        ELSEIF (ICHR.EQ.TSM__K_LESS_THAN) THEN
          COL(ICOMP,ICOL)=COL(ICOMP,ICOL)-INC
          IF (COL(ICOMP,ICOL).LT.0.0) THEN
            COL(ICOMP,ICOL)=0.0
          ENDIF
          CALL ICOLOUR_SHOWVAL(SCREEN.MAIN,ICOL,ICOMP,COL,STATUS)
          CALL ICOLOUR_HIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
          CHANGE=.TRUE.
        ELSEIF (ICHR.GE.TSM__K_ZERO.AND.ICHR.LE.TSM__K_NINE) THEN
          COL(ICOMP,ICOL)=REAL(ICHR-TSM__K_ZERO)/10.0
          CALL ICOLOUR_SHOWVAL(SCREEN.MAIN,ICOL,ICOMP,COL,STATUS)
          CALL ICOLOUR_HIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
          CHANGE=.TRUE.
        ELSEIF (ICHR.GE.TSM__K_KP0.AND.ICHR.LE.TSM__K_KP9) THEN
          COL(ICOMP,ICOL)=REAL(ICHR-TSM__K_KP0)/10.0
          CALL ICOLOUR_SHOWVAL(SCREEN.MAIN,ICOL,ICOMP,COL,STATUS)
          CALL ICOLOUR_HIGHLIGHT(SCREEN.MAIN,ICOL,ICOMP,STATUS)
          CHANGE=.TRUE.
        ENDIF

*  change colours on screen if device active
        IF (CHANGE) THEN

          IF (NCOL.LT.16) THEN
*  less than 16 colours - use every other colour
            CI=FIRST
            DO J=2,16,2
              RED=COL(1,J)
              GREEN=COL(2,J)
              BLUE=COL(3,J)
              CALL PGSCR(CI,RED,GREEN,BLUE)
              CI=CI+1
            ENDDO

          ELSE
*  16 colours - one to one
            IF (NCOL.EQ.16) THEN
              NSHADE=0

*  more than 16 - shade in between
            ELSEIF (NCOL.GT.16) THEN
*  how many shades between main colours
              NSHADE=(NCOL-16)/15

            ENDIF

            J1=MAX(1,ICOL-1)
            J2=MIN(15,ICOL+1)
            CI=FIRST+(J1-1)*(NSHADE+1)
*  set colour
            DO J=J1,J2
              RED=COL(1,J)
              REDHUE=(COL(1,J+1)-RED)/REAL(NSHADE+1)
              GREEN=COL(2,J)
              GREENHUE=(COL(2,J+1)-GREEN)/REAL(NSHADE+1)
              BLUE=COL(3,J)
              BLUEHUE=(COL(3,J+1)-BLUE)/REAL(NSHADE+1)
              CALL PGSCR(CI,RED,GREEN,BLUE)
              DO K=1,NSHADE
                CI=CI+1
                RED=RED+REDHUE
                GREEN=GREEN+GREENHUE
                BLUE=BLUE+BLUEHUE
                CALL PGSCR(CI,RED,GREEN,BLUE)
              ENDDO
              CI=CI+1
            ENDDO
            RED=COL(1,16)
            GREEN=COL(2,16)
            BLUE=COL(3,16)
            CALL PGSCR(LAST,RED,GREEN,BLUE)

          ENDIF



        ENDIF

      ENDDO

 999  CONTINUE

      END




*+
      SUBROUTINE ICOLOUR_HIGHLIGHT(SCREENID,ICOL,ICOMP,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'TSM_PAR'
*    Import :
      INTEGER SCREENID
      INTEGER ICOL,ICOMP
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*5 C5
      INTEGER ROW,COL,I
*-
      ROW=ICOL+2
      COL=ICOMP*10+6
      DO I = 1, 5
        CALL TSM_GETCHAT( SCREENID, COL+I-1, ROW, C5(I:I), STATUS )
      END DO

      CALL TSM_PUTSSTRAT( SCREENID, C5, TSM__REVERSE, COL, ROW, STATUS )
      COL=COL+4
      CALL TSM_MOVCUR( SCREENID, COL, ROW, STATUS )
      CALL TSM_REFRESH( SCREENID, STATUS )

      END



*+
      SUBROUTINE ICOLOUR_UNHIGHLIGHT(SCREENID,ICOL,ICOMP,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      INTEGER SCREENID
      INTEGER ICOL,ICOMP
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      INTEGER ROW,COL,I
      CHARACTER*5 C5
*    Internal References :
*    Local data :
*-

      ROW=ICOL+2
      COL=ICOMP*10+6

      DO I = 1, 5
        CALL TSM_GETCHAT( SCREENID, COL+I-1, ROW, C5(I:I), STATUS )
      END DO

      CALL TSM_PUTSTRAT( SCREENID, C5, COL, ROW, STATUS )
      COL=COL+4
      CALL TSM_MOVCUR( SCREENID, COL, ROW, STATUS )
      CALL TSM_REFRESH( SCREENID, STATUS )

      END


*+
      SUBROUTINE ICOLOUR_SHOWVAL(SCREENID,ICOL,ICOMP,TAB,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER SCREENID
      INTEGER ICOL,ICOMP
      REAL TAB(3,16)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      INTEGER ROW,COL
      CHARACTER*10 STRING
*    Internal References :
*    Local data :
*-

      WRITE(STRING,'(F10.2)') TAB(ICOMP,ICOL)
      ROW=ICOL+2
      COL=ICOMP*10+1
      CALL TSM_PUTSTRAT( SCREENID, STRING, COL, ROW, STATUS )

      END


*+
      SUBROUTINE ICOLOUR_HELP(SCREENID,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER SCREENID
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*25 BLANK
      PARAMETER (BLANK=' ')
*    Local variables :
      CHARACTER*27 HELPTXT(10)
      INTEGER ROW,COL
      INTEGER I
*    Internal References :
*    Local data :
      DATA HELPTXT/'   {up_arrow} - move up',
     &             ' {down_arrow} - move down',
     &             ' {left_arrow} - move left',
     &             '{right_arrow} - move right',
     &             ' ',
     &             '         <    - decrease',
     &             '         >    - increase',
     &             '      {0...9} - set to n/10',
     &             ' ',
     &             '         Q    - quit'/
*-

      ROW=5
      COL=2
      DO I=1,10
        CALL TSM_PUTSTRAT( SCREENID, HELPTXT(I), COL, ROW, STATUS )
        ROW=ROW+1
      ENDDO
      CALL TSM_REFRESH( SCREENID, STATUS )

      END
