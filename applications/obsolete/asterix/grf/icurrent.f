*+  ICURRENT - get current values (ie. pos etc)
      SUBROUTINE ICURRENT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      9 Aug 90: V1.2-1 text output added
*     31 May 91: V1.2-2 Writes NAME parameter
*      5 Jun 91: V1.2-3 screen suppression option added (RJV)
*      4 Dec 91: V1.2-4 more output parameters (RJV)
*     20 Sep 94: V1.7-0 reports on current region (RJV)
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
      CHARACTER*132 NAME
      CHARACTER*80 LINE
      CHARACTER*4 MODE
      DOUBLE PRECISION RA,DEC,ELON,ELAT,L,B
      REAL X1,X2,Y1,Y2
      REAL XPIX,YPIX
      REAL XPIX1,XPIX2,YPIX1,YPIX2
      INTEGER NC
      INTEGER ISTAT
      LOGICAL SUPPRESS
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ICURRENT Version 1.2-4')
*-
*  is screen output required?
      CALL PAR_GET0L('SUPPRESS',SUPPRESS,STATUS)

      IF (.NOT.SUPPRESS) THEN
        CALL MSG_PRNT(VERSION)
      ENDIF

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSE

        IF (.NOT.SUPPRESS) THEN
          CALL MSG_BLNK()
          CALL MSG_PRNT('        '
     :              //'---------Current Image Processing System Status'
     :              //'---------')
          CALL MSG_BLNK()
        ENDIF

*  Name of image file
        CALL STR_OBNAME(I_LOC,NAME,NC,STATUS)
        CALL PAR_PUT0C( 'NAME', NAME(:NC), STATUS )
        IF (.NOT.SUPPRESS) THEN
          CALL MSG_BLNK()
          CALL MSG_PRNT('Processing image: '//NAME(:NC))
          CALL MSG_BLNK()
        ENDIF

*  Current position
        CALL IMG_WORLDTOPIX(I_X,I_Y,XPIX,YPIX,STATUS)
        CALL IMG_WORLDTOCEL(I_X,I_Y,RA,DEC,STATUS)
        CALL IMG_WORLDTOECL(I_X,I_Y,ELON,ELAT,STATUS)
        CALL IMG_WORLDTOGAL(I_X,I_Y,L,B,STATUS)
        CALL PAR_PUT0R('X',I_X,STATUS)			! axis coords
        CALL PAR_PUT0R('Y',I_Y,STATUS)
        CALL PAR_PUT0D('RA',RA,STATUS)			! RA/DEC
        CALL PAR_PUT0D('DEC',DEC,STATUS)
        CALL PAR_PUT0D('ELON',ELON,STATUS)		! Ecliptic
        CALL PAR_PUT0D('ELAT',ELAT,STATUS)
        CALL PAR_PUT0D('L',L,STATUS)			! Galactic
        CALL PAR_PUT0D('B',B,STATUS)

        IF (.NOT.SUPPRESS) THEN
          CALL MSG_PRNT('Current position:')
          LINE=' '
          LINE(21:24)='Xpix='
          LINE(42:45)='Ypix='
          WRITE(LINE(26:36),'(F11.6)',IOSTAT=ISTAT) XPIX
          WRITE(LINE(47:57),'(F11.6)',IOSTAT=ISTAT) YPIX
          CALL MSG_PRNT(LINE)
          LINE=' '
          LINE(23:24)='X='
          LINE(44:45)='Y='
          WRITE(LINE(26:36),'(F11.6)',IOSTAT=ISTAT) I_X
          WRITE(LINE(47:57),'(F11.6)',IOSTAT=ISTAT) I_Y
          CALL MSG_PRNT(LINE)
          LINE=' '
          LINE(22:24)='RA='
          LINE(42:45)='DEC='
          CALL CONV_DEGHMS(REAL(RA),LINE(28:38))
          CALL CONV_DEGDMS(REAL(DEC),LINE(47:57))
          CALL MSG_PRNT(LINE)
          LINE=' '
          LINE(24:24)='='
          LINE(45:45)='='
          WRITE(LINE(26:36),'(F11.6)',IOSTAT=ISTAT) RA
          WRITE(LINE(47:57),'(F11.6)',IOSTAT=ISTAT) DEC
          CALL MSG_PRNT(LINE)
          LINE=' '
          LINE(20:24)='ELON='
          LINE(41:45)='ELAT='
          WRITE(LINE(26:36),'(F11.6)',IOSTAT=ISTAT) ELON
          WRITE(LINE(47:57),'(F11.6)',IOSTAT=ISTAT) ELAT
          CALL MSG_PRNT(LINE)
          LINE=' '
          LINE(23:24)='L='
          LINE(44:45)='B='
          WRITE(LINE(26:36),'(F11.6)',IOSTAT=ISTAT) L
          WRITE(LINE(47:57),'(F11.6)',IOSTAT=ISTAT) B
          CALL MSG_PRNT(LINE)
          CALL MSG_BLNK()
        ENDIF


*  Current region
        XPIX1=REAL(I_IX1)-0.5
        XPIX2=REAL(I_IX2)+0.5
        YPIX1=REAL(I_IY1)-0.5
        YPIX2=REAL(I_IY2)+0.5
        CALL IMG_PIXTOWORLD(XPIX1,YPIX1,X1,Y1,STATUS)
        CALL IMG_PIXTOWORLD(XPIX2,YPIX2,X2,Y2,STATUS)
        CALL PAR_PUT0R('X1',X1,STATUS)
        CALL PAR_PUT0R('X2',X2,STATUS)
        CALL PAR_PUT0R('Y1',Y1,STATUS)
        CALL PAR_PUT0R('Y2',Y2,STATUS)
        CALL PAR_PUT0R('RAD',I_R,STATUS)

        IF (.NOT.SUPPRESS) THEN
          IF (I_REG_TYPE.EQ.'NONE') THEN
            CALL MSG_SETC('REG','WHOLE IMAGE')
          ELSE
            CALL MSG_SETC('REG',I_REG_TYPE)
          ENDIF
          CALL MSG_PRNT('Current region:    ^REG')
        ENDIF

*  Current mode
        IF (I_MODE.EQ.1) THEN
          MODE='CURS'
        ELSE
          MODE='KEY'
        ENDIF
        CALL PAR_PUT0C('MODE',MODE,STATUS)

      ENDIF

      END
