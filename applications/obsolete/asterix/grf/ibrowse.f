*+  IBROWSE - Look at data values around selected point in image
      SUBROUTINE IBROWSE( STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*
*     9 Aug 90 : V1.2-1 Dual units for RA/DEC
*     7 Feb 92 : V1.2-2 Output final central value to parameter
*                       suppress screen output for keyboard mode
*     1 Jul 93 : V1.2-3 GTR used (RJV)
*     9 Sep 93 : V1.2-4 Draws box on image (RJV)
*    20 Dec 93 : V1.2-5 Use TSM for screen management. Use full depth of
*                       text window. (DJA)
*    17 Feb 94 : V1.2-6 New features (RJV)
*    23 May 94 : V1.2-7 Errors and Significance added (RJV)
*     1 Sep 94 : V1.2-8 Positioning error fixed (RJV)
*    25 aug 95 : V1.8-0 GUI interface (RJV)
*    11 Jan 96 : V1.8-1 TSM removed (RJV)
*     2 May 96 : V2.0-0 GUI interface improved (RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
*    Local Constants :
*    Local variables :
      CHARACTER*1 CH
      REAL XW,YW,XXW,YYW
      BYTE Q
      INTEGER DWIN
      INTEGER NCOLS, NROWS
      INTEGER NX,NY
      INTEGER ISCALE
      REAL VAL
      LOGICAL KEYB,FIRST
      LOGICAL VAR,ERR,SIGNIF,QUAL
      LOGICAL LEFT,RIGHT
*    Global Variables :
      INCLUDE 'IMG_CMN'
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='IBROWSE Version 2.0-0')
*-

      CALL MSG_PRNT(VERSION)

      CALL USI_INIT()

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  make sure transformations are correct
        CALL GTR_RESTORE(STATUS)

        IF (I_GUI) THEN

          CALL IBROWSE_GUI(STATUS)


        ELSE

*  find out if data, variance, errors, significance or quality wanted
          VAR=.FALSE.
          QUAL=.FALSE.
          ERR=.FALSE.
          SIGNIF=.FALSE.
          CALL USI_GET0L('VAR',VAR,STATUS)
          IF (VAR) THEN
            IF (.NOT.I_VOK) THEN
              CALL MSG_PRNT(
     :            'AST_ERR: no variance present - defaulting to data')
              VAR=.FALSE.
            ENDIF
          ELSE
            CALL USI_GET0L('ERR',ERR,STATUS)
            IF (ERR) THEN
              IF (.NOT.I_VOK) THEN
                CALL MSG_PRNT(
     :            'AST_ERR: no variance present - defaulting to data')
                ERR=.FALSE.
              ENDIF
            ELSE
              CALL USI_GET0L('SIGNIF',SIGNIF,STATUS)
              IF (SIGNIF) THEN
                IF (.NOT.I_VOK) THEN
                  CALL MSG_PRNT(
     :            'AST_ERR: no variance present - defaulting to data')
                  SIGNIF=.FALSE.
                ENDIF
              ELSE
                CALL USI_GET0L('QUAL',QUAL,STATUS)
                IF (QUAL.AND..NOT.I_QOK) THEN
                  CALL MSG_PRNT(
     :            'AST_ERR: no quality present - defaulting to data')
                  QUAL=.FALSE.
                ENDIF
              ENDIF
            ENDIF
          ENDIF

*  see if data to be scaled
          CALL USI_GET0I('SCALE',ISCALE,STATUS)

*  keyboard mode
          IF (I_MODE.NE.1) THEN
            CALL USI_DEF0R('XPOS',I_X,STATUS)
            CALL USI_DEF0R('YPOS',I_Y,STATUS)
            CALL USI_GET0R('XPOS',XW,STATUS)
            CALL USI_GET0R('YPOS',YW,STATUS)
            KEYB=.TRUE.
            FIRST=.TRUE.
            NX=9
            NY=9
            CALL IBROWSE_DISP(FIRST,KEYB,DWIN,NROWS,NX,NY,VAR,ERR,
     :                          SIGNIF,QUAL,ISCALE,XW,YW,VAL,Q,STATUS)

          ELSE

            CALL MSG_PRNT(' ')
            CALL MSG_PRNT('Select points (press X to exit)...')

            FIRST=.TRUE.
            KEYB=.FALSE.
            XW=I_X
            YW=I_Y
            XXW=XW
            YYW=YW


            CH=' '
            DO WHILE (CH.NE.'X'.AND.STATUS.EQ.SAI__OK)

              CALL GFX_CURS(XW,YW,LEFT,RIGHT,CH,STATUS)

              IF (CH.EQ.'X') THEN
                XW=XXW
                YW=YYW
              ELSE


                NX = 9
                NY = 9


                IF (CH.EQ.'D') THEN
                  XW=XXW
                  YW=YYW
                  VAR=.FALSE.
                  QUAL=.FALSE.
                  ERR=.FALSE.
                  SIGNIF=.FALSE.
                ELSEIF (CH.EQ.'V'.AND.I_VOK) THEN
                  XW=XXW
                  YW=YYW
                  VAR=.TRUE.
                  QUAL=.FALSE.
                  ERR=.FALSE.
                  SIGNIF=.FALSE.
                ELSEIF (CH.EQ.'E'.AND.I_VOK) THEN
                  XW=XXW
                  YW=YYW
                  VAR=.FALSE.
                  QUAL=.FALSE.
                  ERR=.TRUE.
                  SIGNIF=.FALSE.
                ELSEIF (CH.EQ.'S'.AND.I_VOK) THEN
                  XW=XXW
                  YW=YYW
                  VAR=.FALSE.
                  QUAL=.FALSE.
                  ERR=.FALSE.
                  SIGNIF=.TRUE.
                ELSEIF (CH.EQ.'Q'.AND.I_QOK) THEN
                  XW=XXW
                  YW=YYW
                  VAR=.FALSE.
                  QUAL=.TRUE.
                  ERR=.FALSE.
                  SIGNIF=.FALSE.
                ELSEIF (CH.EQ.'<') THEN
                  XW=XXW
                  YW=YYW
                  ISCALE=ISCALE-1
                ELSEIF (CH.EQ.'>') THEN
                  XW=XXW
                  YW=YYW
                  ISCALE=ISCALE+1
                ENDIF

                CALL IBROWSE_DISP(FIRST,KEYB,DWIN,NROWS,NX,NY,VAR,ERR,
     :                           SIGNIF,QUAL,ISCALE,XW,YW,VAL,Q,STATUS)
                XXW=XW
                YYW=YW
                FIRST=.FALSE.

              ENDIF
            ENDDO


          ENDIF

*  write final value to parameter and reset current position
          CALL IMG_SETPOS(XW,YW,STATUS)
          IF (QUAL) THEN
            CALL USI_PUT0I('VAL',Q,STATUS)
          ELSE
            CALL USI_PUT0R('VAL',VAL,STATUS)
          ENDIF

        ENDIF

      ENDIF

  99  CONTINUE

      CALL USI_CLOSE()

      END



*+  IBROWSE_DISP
      SUBROUTINE IBROWSE_DISP(FIRST,KEYB,DWIN,NROWS,NX,NY,VAR,ERR,
     :                          SIGNIF,QUAL,ISCALE,XW,YW,VAL,Q,STATUS)
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
      INCLUDE 'DAT_PAR'
*    Import :
      LOGICAL KEYB
      LOGICAL FIRST
      INTEGER DWIN
      INTEGER NROWS
      INTEGER NX,NY
      INTEGER ISCALE
      LOGICAL VAR,QUAL,ERR,SIGNIF
      REAL XW,YW
*    Import-Export :
*    Export :
      REAL VAL
      BYTE Q
*    Status :
      INTEGER STATUS
*    Functions :
*    Local Constants :
*    Local variables :
      REAL XP,YP
      CHARACTER*80 STRING
      CHARACTER*10 FMT
      CHARACTER*79 LIN1
      CHARACTER*24 XYSTR
      CHARACTER*24 RASTR
      CHARACTER*22 DECSTR
      CHARACTER*40  ESTR
      CHARACTER*39  GSTR
      CHARACTER*39 CMD1
      CHARACTER*39 CMD2
      CHARACTER*8 STR8
      INTEGER IX,IY,I,J
      INTEGER I1,I2,J1,J2
      INTEGER IC1,IC2
      INTEGER ROW,CPOS
      INTEGER IPIX(2)
      INTEGER ISTAT
      REAL SCVAL,VAL2
      REAL XC,YC,DX,DY
      DOUBLE PRECISION RA,DEC,ELON,ELAT,GLON,GLAT
*    Global Variables :
      INCLUDE 'IMG_CMN'
*    Local data:
      DATA LIN1
     :  /'       Pixel                  - values scaled by 1.0 '/
      DATA XYSTR/' X=         Y='/
      DATA RASTR/'RA=          /'/
      DATA DECSTR/'DEC=         /'/
      DATA  ESTR/' Ecl.long=          Ecl.lat='/
      DATA  GSTR/' Gal.long=          Gal.lat='/
      DATA CMD1/' X-eXit  D-Data  V-Variance  E-Error  '/
      DATA CMD2/'S-Signif  Q-Quality   ><-scale up/down'/
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  blank out previous box
        IF (.NOT.FIRST) THEN
          I1=MAX(1,I1-1)
          I2=MIN(I_NX,I2+1)
          J1=MAX(1,J1-1)
          J2=MIN(I_NY,J2+1)
          CALL IMG_SUBPIXEL(I1,I2,J1,J2,STATUS)
        ENDIF

*  Label the window
        STRING=' '
        IF ( VAR ) THEN
          STRING(35:)='Variances'
        ELSEIF (QUAL) THEN
          STRING(32:)='Quality flags'
        ELSEIF (ERR) THEN
          STRING(36:)='Errors'
        ELSEIF (SIGNIF) THEN
          STRING(33:)='Significances'
        ELSE
          STRING(34:)='Data values'
        ENDIF

        CALL MSG_PRNT(STRING)

*  convert to other frames
        CALL IMG_WORLDTOPIX(XW,YW,XP,YP,STATUS)
        CALL IMG_WORLDTOCEL(XW,YW,RA,DEC,STATUS)
        CALL IMG_WORLDTOECL(XW,YW,ELON,ELAT,STATUS)
        CALL IMG_WORLDTOGAL(XW,YW,GLON,GLAT,STATUS)

*  get pixel number
        IX=INT(XP+0.5)
        IY=INT(YP+0.5)
        IPIX(1)=IX
        IPIX(2)=IY

*  get centre and size of box in world coords and pixels
        CALL IMG_PIXTOWORLD(REAL(IX),REAL(IY),XC,YC,STATUS)
        DX=REAL(NX)*ABS(I_XSCALE)/2.0
        DY=REAL(NY)*ABS(I_YSCALE)/2.0
        I1=MAX(I_IX1,IX-NX/2)
        I2=MIN(I_IX2,IX+NX/2)
        J1=MAX(I_IY1,IY-NY/2)
        J2=MIN(I_IY2,IY+NY/2)
*  show box
        CALL IMG_BOX(XC,YC,DX,DY,STATUS)

*  display data values
        ROW=2
        STRING=' '

        DO J=J2,J1,-1

          IC1=1
          IC2=8
          DO I=I1,I2

            IF (J.LT.I_IY1.OR.J.GT.I_IY2.OR.
     :                      I.LT.I_IX1.OR.I.GT.I_IX2) THEN
              STRING(IC1:IC2)=' '
            ELSE
              IF (VAR) THEN
                CALL IMG_GETVAR(I,J,VAL,STATUS)
                SCVAL=VAL*10.0**ISCALE
                CALL IBROWSE_FMT(SCVAL,FMT)
                WRITE(STRING(IC1:IC2),FMT,IOSTAT=ISTAT) SCVAL
              ELSEIF (ERR) THEN
                CALL IMG_GETVAR(I,J,VAL,STATUS)
                IF (VAL.GT.0.0) THEN
                  VAL=SQRT(VAL)
                ELSE
                  VAL=0.0
                ENDIF
                SCVAL=VAL*10.0**ISCALE
                CALL IBROWSE_FMT(SCVAL,FMT)
                WRITE(STRING(IC1:IC2),FMT,IOSTAT=ISTAT) SCVAL
              ELSEIF (SIGNIF) THEN
                CALL IMG_GETVAL(I,J,VAL,STATUS)
                CALL IMG_GETVAR(I,J,VAL2,STATUS)
                IF (VAL2.GT.0.0) THEN
                  VAL=VAL/SQRT(VAL2)
                ELSE
                  VAL=0.0
                ENDIF
                SCVAL=VAL*10.0**ISCALE
                CALL IBROWSE_FMT(SCVAL,FMT)
                WRITE(STRING(IC1:IC2),FMT,IOSTAT=ISTAT) SCVAL
              ELSEIF (QUAL) THEN
                CALL IMG_GETQUAL(I,J,Q,STATUS)
                CALL STR_BTOC(Q,STRING(IC1:IC2),STATUS)
              ELSE
                CALL IMG_GETVAL(I,J,VAL,STATUS)
                SCVAL=VAL*10.0**ISCALE
                CALL IBROWSE_FMT(SCVAL,FMT)
                WRITE(STRING(IC1:IC2),FMT,IOSTAT=ISTAT) SCVAL
              ENDIF
            ENDIF

            IC1=IC1+8
            IC2=IC2+8

          ENDDO

          CALL MSG_PRNT(STRING)
          CALL MSG_BLNK()

          ROW=ROW+2

        ENDDO


*  display positions
        CALL STR_DIMTOC(2,IPIX,LIN1(14:30))
        LIN1(53:57)=' '
        IF (ISCALE.GT.0) THEN
          LIN1(53:53)='E'
          WRITE(LIN1(54:55),'(I2.2)') ISCALE
        ELSEIF (ISCALE.LT.0) THEN
          LIN1(53:53)='E'
          WRITE(LIN1(54:56),'(I3.2)') ISCALE
        ENDIF

        CALL IBROWSE_FMT(XW,FMT)
        WRITE(XYSTR(4:11),FMT,IOSTAT=ISTAT) XW
        CALL IBROWSE_FMT(YW,FMT)
        WRITE(XYSTR(15:22),FMT,IOSTAT=ISTAT) YW

        CALL CONV_DEGHMS(REAL(RA),RASTR(4:13))
        WRITE(RASTR(15:22),'(F8.4)',IOSTAT=ISTAT) REAL(RA)
        CALL CONV_DEGDMS(REAL(DEC),DECSTR(5:13))
        WRITE(DECSTR(15:22),'(F8.4)',IOSTAT=ISTAT) REAL(DEC)
        WRITE(ESTR(11:18),'(F8.4)') REAL(ELON)
        WRITE(ESTR(29:36),'(F8.4)') REAL(ELAT)
        WRITE(GSTR(11:18),'(F8.4)') REAL(GLON)
        WRITE(GSTR(29:36),'(F8.2)') REAL(GLAT)

        CALL MSG_BLNK()
        CALL MSG_PRNT(LIN1)
        CALL MSG_PRNT(XYSTR//RASTR//DECSTR)
        CALL MSG_PRNT(ESTR//GSTR)


      ENDIF

      END



*+
      SUBROUTINE IBROWSE_FMT(X,FMT)

      REAL X
      CHARACTER*(*) FMT

      REAL XX
*-
      XX=ABS(X)

*  set format according to magnitude of data
      IF     (XX.GT.1.0E5) THEN
        FMT='(1P,E8.2)'
      ELSEIF (XX.GT.1.0E4) THEN
        FMT='(F8.1)'
      ELSEIF (XX.GT.1.0E3) THEN
        FMT='(F8.2)'
      ELSEIF (XX.GT.1.0E2) THEN
        FMT='(F8.3)'
      ELSEIF (XX.GT.10.0) THEN
        FMT='(F8.4)'
      ELSE
        FMT='(F8.5)'
      ENDIF

      END






*+  IBROWSE_GUI
      SUBROUTINE IBROWSE_GUI(STATUS)
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
*    Local Constants :
      INTEGER NX,NY
      PARAMETER (NX=9,NY=9)
*    Local variables :
      REAL X,Y
      REAL XP,YP
      CHARACTER*8 NAME
      CHARACTER*8 STRING
      CHARACTER*10 FMT
      CHARACTER*12 OPTIONS
      BYTE Q
      INTEGER IXP,IYP,IXPMAX,IYPMAX
      INTEGER IX,IY
      INTEGER I,J,II,JJ
      INTEGER I1,I2,J1,J2
      INTEGER ISTAT
      INTEGER DID(NX,NY)
      INTEGER RAID,DECID
      INTEGER FLAG
      INTEGER ISCALE
      INTEGER NB
      REAL XP1,XP2,YP1,YP2
      REAL XW,YW
      REAL XW1,XW2,YW1,YW2
      REAL SCVAL,VAL,VAL2
      REAL XC,YC,DX,DY
      REAL XSCALE,YSCALE
      LOGICAL VAR,ERR,SIGNIF,QUAL
*    Global Variables :
      INCLUDE 'IMG_CMN'
*-

      IF (STATUS.EQ.SAI__OK) THEN

        X=I_X
        Y=I_Y

*  locate noticeboard items
        NAME='DATA'
        DO I=1,NX
          DO J=1,NY
            IF (I.LT.10) THEN
              WRITE(NAME(5:5),'(I1)') I
            ELSE
              WRITE(NAME(5:6),'(I2)') I
            ENDIF
            IF (J.LT.10) THEN
              WRITE(NAME(6:6),'(I1)') J
            ELSE
              WRITE(NAME(6:7),'(I2)') J
            ENDIF
            CALL NBS_FIND_ITEM(I_NBID,NAME,DID(I,J),STATUS)
          ENDDO
        ENDDO

*  get device size
        CALL IMG_NBGET0I('XPMAX',IXPMAX,STATUS)
        CALL IMG_NBGET0I('YPMAX',IYPMAX,STATUS)

*  get plot window parameters
        CALL PGQVP(3,XP1,XP2,YP1,YP2)
        CALL PGQWIN(XW1,XW2,YW1,YW2)

        XSCALE=(XW2-XW1)/(XP2-XP1)
        YSCALE=(YW2-YW1)/(YP2-YP1)

        FLAG=1
*  wait for signal to start browsing
        DO WHILE (FLAG.NE.0)
          CALL IMG_NBGET0I('FLAG',FLAG,STATUS)
        ENDDO
        DO WHILE (FLAG.EQ.0)

*  get current display option
          CALL IMG_NBGET0C('OPTIONS',OPTIONS,STATUS)
          VAR=.FALSE.
          ERR=.FALSE.
          SIGNIF=.FALSE.
          QUAL=.FALSE.
          ISCALE=0
          IF (INDEX(OPTIONS,'Q').NE.0) THEN
            QUAL=.TRUE.
          ELSEIF (INDEX(OPTIONS,'V').NE.0) THEN
            VAR=.TRUE.
          ELSEIF (INDEX(OPTIONS,'E').NE.0) THEN
            ERR=.TRUE.
          ELSEIF (INDEX(OPTIONS,'S').NE.0) THEN
            SIGNIF=.TRUE.
          ENDIF

*  get current cursor position in device coords
          CALL IMG_NBGET0I('XP',IXP,STATUS)
          CALL IMG_NBGET0I('YP',IYP,STATUS)
          IYP=IYPMAX-IYP

*  convert to world coords
          XW=XW1+(REAL(IXP)-XP1)*XSCALE
          YW=YW1+(REAL(IYP)-YP1)*YSCALE


*  get pixel numbers for box being inspected
          CALL IMG_WORLDTOPIX(XW,YW,XP,YP,STATUS)
          IX=INT(XP+0.5)
          IY=INT(YP+0.5)

          I1=IX-NX/2
          I2=IX+NX/2
          J1=IY-NY/2
          J2=IY+NY/2

*  write data values to noticeboard
          STRING=' '

          JJ=0
          DO J=J1,J2

            JJ=JJ+1
            II=0

            DO I=I1,I2

              II=II+1
              STRING=' '
              IF (J.GE.I_IY1.AND.J.LE.I_IY2.AND.
     :                      I.GE.I_IX1.AND.I.LE.I_IX2) THEN

                IF (VAR.AND.I_VOK) THEN
                  CALL IMG_GETVAR(I,J,VAL,STATUS)
                  SCVAL=VAL*10.0**ISCALE
                  CALL IBROWSE_FMT(SCVAL,FMT)
                  WRITE(STRING,FMT,IOSTAT=ISTAT) SCVAL
                ELSEIF (ERR.AND.I_VOK) THEN
                  CALL IMG_GETVAR(I,J,VAL,STATUS)
                  IF (VAL.GT.0.0) THEN
                    VAL=SQRT(VAL)
                  ELSE
                    VAL=0.0
                  ENDIF
                  SCVAL=VAL*10.0**ISCALE
                  CALL IBROWSE_FMT(SCVAL,FMT)
                  WRITE(STRING,FMT,IOSTAT=ISTAT) SCVAL
                ELSEIF (SIGNIF.AND.I_VOK) THEN
                  CALL IMG_GETVAL(I,J,VAL,STATUS)
                  CALL IMG_GETVAR(I,J,VAL2,STATUS)
                  IF (VAL2.GT.0.0) THEN
                    VAL=VAL/SQRT(VAL2)
                  ELSE
                    VAL=0.0
                  ENDIF
                  SCVAL=VAL*10.0**ISCALE
                  CALL IBROWSE_FMT(SCVAL,FMT)
                  WRITE(STRING,FMT,IOSTAT=ISTAT) SCVAL
                ELSEIF (QUAL.AND.I_QOK) THEN
                  CALL IMG_GETQUAL(I,J,Q,STATUS)
                  CALL STR_BTOC(Q,STRING,STATUS)
                ELSE
                  CALL IMG_GETVAL(I,J,VAL,STATUS)
                  SCVAL=VAL*10.0**ISCALE
                  CALL IBROWSE_FMT(SCVAL,FMT)
                  WRITE(STRING,FMT,IOSTAT=ISTAT) SCVAL
                ENDIF
              ENDIF

              CALL NBS_PUT_CVALUE(DID(II,JJ),0,STRING,STATUS)

            ENDDO

          ENDDO

          CALL IMG_SETPOS(XW,YW,STATUS)
          CALL IMG_NBGET0I('FLAG',FLAG,STATUS)

        ENDDO

        CALL IMG_SETPOS(X,Y,STATUS)

      ENDIF

      END
