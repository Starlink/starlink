*+ ISLICE - takes a 1-d slice from an image
      SUBROUTINE ISLICE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*      7 Jun 90: V1.2-1  trap zero width
*      3 Aug 90: V1.2-2  keyboard mode
*      8 May 91: V1.2-3  fixed bug in angle (RJV)
*      5 Dec 91: V1.2-4  default for cursor mode (RJV)
*      7 Aug 92: V1.2-5  handles QUALITY (RJV)
*     20 Jan 93: V1.7-0  uses GCB etc (RJV)
*      1 Jul 93: V1.7-1  GTR used (RJV)
*      9 Aug 94: V1.7-2  really handles QUALITY (RJV)
*     31 Jan 95: V1.8-0  bug fix to keyboard mode (RJV)
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
      REAL PI,DTOR
      PARAMETER (PI=3.14159265,DTOR=PI/180.0)
*    Local variables :
      CHARACTER*1 CH
      REAL XCENT,YCENT
      REAL PXCENT,PYCENT
      REAL XEND,YEND
      REAL PXEND,PYEND
      REAL XOEND,YOEND
      REAL PXOEND,PYOEND
      REAL XWID,YWID
      REAL PXWID,PYWID
      REAL LENGTH,HLEN
      REAL WIDTH,HWID
      REAL PLENGTH,PHLEN,PWIDTH,PHWID
      REAL ANGLE,ALPHA,BETA
      REAL D
      REAL XTR,YTR,XTL,YTL,XBR,YBR,XBL,YBL
      REAL PXTR,PYTR,PXTL,PYTL,PXBR,PYBR,PXBL,PYBL
      INTEGER WPNTR
      INTEGER LS
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ISLICE Version 1.8-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image being displayed')

      ELSE

*  ensure transformations correct
        CALL GTR_RESTORE(STATUS)
        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

*  get centre of cut

        IF (I_MODE.EQ.1) THEN
          CALL MSG_PRNT(' ')
          XCENT=I_X
          YCENT=I_Y
          CALL MSG_SETR('X',XCENT)
          CALL MSG_SETR('Y',YCENT)
          CALL MSG_PRNT('Select centre/^X,^Y/...')
          CALL PGCURSE(XCENT,YCENT,CH)
          IF (CH.EQ.CHAR(13).OR.CH.EQ.CHAR(50)) THEN
            XCENT=I_X
            YCENT=I_Y
          ENDIF
          CALL PGPOINT(1,XCENT,YCENT,2)
          CALL IMG_WORLDTOPIX(XCENT,YCENT,PXCENT,PYCENT,STATUS)

*  get mid-point of end
          CALL MSG_PRNT('Select end...')
          XEND=XCENT
          YEND=YCENT
          CALL PGCURSE(XEND,YEND,CH)
          CALL PGPOINT(1,XEND,YEND,2)
          CALL IMG_WORLDTOPIX(XEND,YEND,PXEND,PYEND,STATUS)

*  calculate other end and draw centre line
          XOEND=2.0*XCENT-XEND
          YOEND=2.0*YCENT-YEND
          CALL IMG_WORLDTOPIX(XOEND,YOEND,PXOEND,PYOEND,STATUS)
          CALL PGPOINT(1,XOEND,YOEND,2)
          CALL PGQLS(LS)
          CALL PGSLS(2)
          CALL PGDRAW(XEND,YEND)

*  get width
          CALL MSG_PRNT('Select width...')
          XWID=XCENT
          YWID=YCENT
          CALL PGCURSE(XWID,YWID,CH)
          CALL IMG_WORLDTOPIX(XWID,YWID,PXWID,PYWID,STATUS)

*  calc length
          PHLEN=SQRT((PXEND-PXCENT)**2 + (PYEND-PYCENT)**2)
          PHLEN=MAX(1.0,PHLEN)
          PLENGTH=PHLEN*2.0

*  calc angle
          ANGLE=ATAN2((PYEND-PYCENT),(PXEND-PXCENT))

*  calc width
          D=SQRT((PXWID-PXCENT)**2 + (PYWID-PYCENT)**2)
          ALPHA=ATAN2((PYWID-PYCENT),(PXWID-PXCENT))
          BETA=ALPHA-ANGLE
          PHWID=ABS(D*SIN(BETA))
          PHWID=MAX(0.5,PHWID)
          PWIDTH=PHWID*2.0

*  keyboard mode
        ELSE
          CALL USI_DEF0R('XCENT',I_X,STATUS)
          CALL USI_GET0R('XCENT',XCENT,STATUS)
          CALL USI_DEF0R('YCENT',I_Y,STATUS)
          CALL USI_GET0R('YCENT',YCENT,STATUS)
          CALL USI_GET0R('ANGLE',ANGLE,STATUS)
          CALL USI_GET0R('LENGTH',LENGTH,STATUS)
          CALL USI_GET0R('WIDTH',WIDTH,STATUS)
          ANGLE=ANGLE*DTOR
*  convert to pixel coords
          CALL IMG_WORLDTOPIX(XCENT,YCENT,PXCENT,PYCENT,STATUS)
c          PLENGTH = LENGTH/(ABS(I_XSCALE*COS(ANGLE)) +
c     :                        ABS(I_YSCALE*SIN(ANGLE)))
          PLENGTH=LENGTH/ABS(I_XSCALE)
          PLENGTH=MAX(2.0,PLENGTH)
c          PWIDTH = WIDTH/(ABS(I_XSCALE*SIN(ANGLE))   +
c     :                        ABS(I_YSCALE*COS(ANGLE)))
          PWIDTH=WIDTH/ABS(I_XSCALE)
          PWIDTH=MAX(1.0,PWIDTH)
          PHWID=PWIDTH/2.0
          PHLEN=PLENGTH/2.0
          PXEND=PXCENT+PHLEN*COS(ANGLE)
          PXOEND=PXCENT-PHLEN*COS(ANGLE)
          PYEND=PYCENT+PHLEN*SIN(ANGLE)
          PYOEND=PYCENT-PHLEN*SIN(ANGLE)
        ENDIF

*  plot extent of cut
        PXTR=PXEND+PHWID*SIN(ANGLE)
        PYTR=PYEND-PHWID*COS(ANGLE)
        PXTL=PXEND-PHWID*SIN(ANGLE)
        PYTL=PYEND+PHWID*COS(ANGLE)
        PXBR=PXOEND+PHWID*SIN(ANGLE)
        PYBR=PYOEND-PHWID*COS(ANGLE)
        PXBL=PXOEND-PHWID*SIN(ANGLE)
        PYBL=PYOEND+PHWID*COS(ANGLE)
        CALL IMG_PIXTOWORLD(PXBR,PYBR,XBR,YBR,STATUS)
        CALL IMG_PIXTOWORLD(PXBL,PYBL,XBL,YBL,STATUS)
        CALL IMG_PIXTOWORLD(PXTL,PYTL,XTL,YTL,STATUS)
        CALL IMG_PIXTOWORLD(PXTR,PYTR,XTR,YTR,STATUS)
        CALL PGMOVE(XBR,YBR)
        CALL PGDRAW(XBL,YBL)
        CALL PGDRAW(XTL,YTL)
        CALL PGDRAW(XTR,YTR)
        CALL PGDRAW(XBR,YBR)

*  create and map data array
        I_N_1D=INT(PLENGTH)
        CALL IMG_GET1D(I_N_1D,STATUS)

*  map a temporary workspace array
        CALL DYN_MAPI(1,I_N_1D,WPNTR,STATUS)

*  make cut
        CALL ISLICE_DOIT(%VAL(I_DPTR),%VAL(I_VPTR),%VAL(I_QPTR),
     :               PXCENT,PYCENT,PHWID,PHLEN,ANGLE,%VAL(WPNTR),
     :                            %VAL(I_DPTR_1D),%VAL(I_VPTR_1D),
     :                                     %val(I_QPTR_1D),STATUS)
*

        CALL DYN_UNMAP(WPNTR,STATUS)

*  reset auxiliary plot
        I_N_AUX=0

*  axis and labels
        I_XBASE_1D=0.5
        I_XSCALE_1D=1.0
        I_XWID_1D=1.0
        I_XLABEL_1D='Slice position'
        I_XUNITS_1D='pixels'
        I_LABEL_1D='Surface brightness'
        I_UNITS_1D='Counts/sec/pixel'
        I_TITLE_1D='Projected slice'
        CALL ARR_REG1R(I_XBASE_1D,I_XSCALE_1D,I_N_1D,%VAL(I_APTR_1D),
     :                                                          STATUS)
        CALL ARR_INIT1R(ABS(I_XSCALE_1D),I_N_1D,%VAL(I_WPTR_1D),STATUS)

*  set default axis ranges
        I_X1_1D=0.0
        I_X2_1D=REAL(I_N_1D)+0.5
        CALL ARR_RANG1R(I_N_1D,%VAL(I_DPTR_1D),I_Y1_1D,I_Y2_1D,STATUS)
        I_Y2_1D=I_Y2_1D*1.1

        CALL GDV_CLEAR(STATUS)

*  uncache 1D GCB
        CALL IMG_1DGCB(STATUS)

*  set plotting style
        CALL GCB_SETL('ERR_FLAG',.TRUE.,STATUS)
        CALL GCB_SETL('STEP_FLAG',.FALSE.,STATUS)
        CALL GCB_SETL('POLY_FLAG',.FALSE.,STATUS)
        CALL GCB_SETL('POINT_FLAG',.FALSE.,STATUS)

        CALL IMG_PLOT(STATUS)

*  flag current plotting status
        I_DISP_1D=.TRUE.
        I_DISP=.FALSE.
        I_CLEAR=.FALSE.

      ENDIF

      CALL USI_CLOSE()

      END





*+ ISLICE_DOIT	Extract a cut profile from an image array
	SUBROUTINE ISLICE_DOIT(ARRAY,VAR,Q,CUTX,CUTY,HWID,HLEN,THETA,COUNT,
     :                                          CUT,CVAR,CQUAL,STATUS)
*    Description :
*       Takes a cut from a 2-d array. If an input variance array was
*      present then it uses this to calculate the output variances
*      otherwise variance is found from counting statistics.
*    History :
*       Author Dick Willingale 1988-Oct-26
*       Asterix88 version   1989-MAY-23          (LTVAD::RDS)
*    Type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'DAT_PAR'
        INCLUDE 'QUAL_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*    Import :
	REAL ARRAY(I_NX,I_NY)                        ! Input data array
	REAL VAR(I_NX,I_NY)                          ! Variance array
        BYTE Q(I_NX,I_NY)			     ! QUALITY array
        REAL CUTX,CUTY                               ! Centre of cut
        REAL HWID                                    ! Half width of cut
        REAL HLEN                                    ! Half length of cut
        REAL THETA                                   ! Angle of cut
        INTEGER COUNT(I_N_1D)                        ! Count workspace
*    Import-Export :
        REAL CUT(I_N_1D)                             ! Output cut array
        REAL CVAR(I_N_1D)                            ! Output variance array
        BYTE CQUAL(I_N_1D)			     ! Output quality array
*    Export :
*    Status :
        INTEGER STATUS
*    Function declarations :
        BYTE BIT_ANDUB,BIT_ORUB
*    Local variables :
        REAL CTH,STH                                 ! Cos & sin theta
        REAL CLENGTH                                 ! Full cut length
        REAL CSAM                                    ! Length per sample
        INTEGER IXP,IYP,IRAD
        INTEGER NXL,NXH,NYL,NYH                      ! Pixel extents to use
        INTEGER J,K,II
        REAL XOLD,YOLD,XNEW,YNEW
        REAL RNN,RNN2                                ! Normalising factors
        LOGICAL GOOD
*-
        IF (STATUS.NE.SAI__OK) RETURN
C
	CTH=COS(THETA)
	STH=SIN(THETA)
C Find length and sample size
	CLENGTH=HLEN*2.0
	CSAM=CLENGTH/REAL(I_N_1D)
C Initialize output arrays
	DO J=1,I_N_1D
          COUNT(J)=0.0
          CUT(J)=0.0
	  CVAR(J)=0.0
          CQUAL(J)=QUAL__GOOD
	ENDDO
C Find pixel ranges about centre
	IXP=CUTX+1.0
	IYP=CUTY+1.0
	IRAD=SQRT(HLEN**2+HWID**2)+2.0
	NXL=MAX(MIN(IXP-IRAD,I_NX),1)
	NXH=MAX(MIN(IXP+IRAD,I_NX),1)
	NYL=MAX(MIN(IYP-IRAD,I_NY),1)
	NYH=MAX(MIN(IYP+IRAD,I_NY),1)
*
C Now scan image array
	DO J=NYL,NYH
	  YOLD=(REAL(J)-0.5)-CUTY
	  DO K=NXL,NXH
	    XOLD=(REAL(K)-0.5)-CUTX
	    YNEW=YOLD*CTH-XOLD*STH
	    XNEW=YOLD*STH+XOLD*CTH
	    IF (ABS(YNEW).LE.HWID.AND.ABS(XNEW).LE.HLEN) THEN
	      II=(XNEW+HLEN)/CSAM+1.0
	      IF (II.GT.0.AND.II.LE.I_N_1D) THEN
                IF (I_QOK) THEN
                  GOOD=(BIT_ANDUB(Q(K,J),I_MASK).EQ.QUAL__GOOD)
                ELSE
                  GOOD=.TRUE.
                ENDIF
                IF (GOOD) THEN
                  COUNT(II)=COUNT(II)+1
                  CUT(II)=CUT(II)+ARRAY(K,J)
                  IF (I_VOK) THEN
	            CVAR(II)=CVAR(II)+VAR(K,J)
                  ENDIF
                  CQUAL(II)=QUAL__GOOD
                ELSE
                  IF (COUNT(II).EQ.0) THEN
                    CQUAL(II)=BIT_ORUB(Q(K,J),CQUAL(II))
                  ENDIF
                ENDIF
	      ENDIF
	    ENDIF
	  ENDDO
	ENDDO
C Normalise to surface brightness per pixel
	DO J=1,I_N_1D
	  IF (COUNT(J).LE.0) THEN
C Mark gaps
            CVAR(J)=0.0
	    CUT(J)=0.0
	  ELSE
C Normalise and calculate standard deviations
	    RNN=1./COUNT(J)
            RNN2=RNN*RNN
*
	    IF( .NOT. I_VOK) THEN
	      CVAR(J)=CUT(J)*RNN2
	    ELSE
	      CVAR(J)=CVAR(J)*RNN2
	    ENDIF
	    CUT(J)=CUT(J)*RNN
*
	  ENDIF
	ENDDO
*
	END
