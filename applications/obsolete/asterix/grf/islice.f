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
*     31 Jan 95: V1.8-1  axis units pixels->angle (RJV)
*     11 dec 95: V2.0-0  GUI version (RJV)
*     23 May 97: V2.1-0  GUI form entry (rjv)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
      REAL PI,DTOR
      PARAMETER (PI=3.14159265,DTOR=PI/180.0)
*    Local variables :
      CHARACTER*12 OPT
      REAL XCENT,YCENT
      REAL PXCENT,PYCENT
      REAL PXEND,PYEND
      REAL PXOEND,PYOEND
      REAL LENGTH
      REAL WIDTH
      REAL PLENGTH,PHLEN,PWIDTH,PHWID
      REAL ANGLE
      INTEGER WPNTR
      INTEGER L
      INTEGER OID,NB
      LOGICAL PLOT
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ISLICE Version 2.2-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image being displayed')

      ELSE

*  if run from GUI - is plot required
        IF (I_GUI) THEN
          CALL NBS_FIND_ITEM(I_NBID,'OPTIONS',OID,STATUS)
          CALL NBS_GET_CVALUE(OID,0,OPT,NB,STATUS)
*  GUI is plotting externally - so no plot here
          IF (OPT(1:1).EQ.'E') THEN
            PLOT=.FALSE.
          ELSE
            PLOT=.TRUE.
          ENDIF

        ELSE
          PLOT=.TRUE.
        ENDIF


*  ensure transformations correct
        CALL GTR_RESTORE(STATUS)
        CALL GCB_ATTACH('IMAGE',STATUS)



*  get slice parameters
        CALL IMG_GETSLICE('XCENT','YCENT','ANGLE','LENGTH','WIDTH',
     :                        XCENT,YCENT,ANGLE,LENGTH,WIDTH,STATUS)

*  convert to pixel coords
        CALL IMG_WORLDTOPIX(XCENT,YCENT,PXCENT,PYCENT,STATUS)
        PLENGTH=LENGTH/ABS(I_XSCALE)
        PLENGTH=MAX(2.0,PLENGTH)
        PWIDTH=WIDTH/ABS(I_XSCALE)
        PWIDTH=MAX(1.0,PWIDTH)
        PHWID=PWIDTH/2.0
        PHLEN=PLENGTH/2.0
        PXEND=PXCENT+PHLEN*COS(ANGLE)
        PXOEND=PXCENT-PHLEN*COS(ANGLE)
        PYEND=PYCENT+PHLEN*SIN(ANGLE)
        PYOEND=PYCENT-PHLEN*SIN(ANGLE)

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
        I_XSCALE_1D=ABS(I_XSCALE)
        I_XBASE_1D=0.5*I_XSCALE_1D
        I_XWID_1D=I_XSCALE_1D
        I_XLABEL_1D='Distance along slice'
        I_XUNITS_1D=I_XYUNITS
        I_LABEL_1D='Surface brightness'
        I_UNITS_1D=I_UNITS
        L=CHR_LEN(I_UNITS)
        L=L+1
        I_UNITS_1D(L:)='/'//I_XYUNITS
        L=CHR_LEN(I_UNITS_1D)
        IF (I_UNITS_1D(L:L).NE.'S'.AND.I_UNITS_1D(L:L).NE.'s') THEN
          L=L+1
        ENDIF
        I_UNITS_1D(L:)=CHAR(92)//'u2'//CHAR(92)//'d'
        I_TITLE_1D='Projected slice'
        CALL ARR_REG1R(I_XBASE_1D,I_XSCALE_1D,I_N_1D,%VAL(I_APTR_1D),
     :                                                          STATUS)
        CALL ARR_INIT1R(ABS(I_XSCALE_1D),I_N_1D,%VAL(I_WPTR_1D),STATUS)

*  set default axis ranges
        I_X1_1D=0.0
        I_X2_1D=(REAL(I_N_1D)+0.5)*I_XSCALE_1D
        CALL ARR_RANG1R(I_N_1D,%VAL(I_DPTR_1D),I_Y1_1D,I_Y2_1D,STATUS)
        I_Y2_1D=I_Y2_1D*1.1


*  uncache 1D GCB
        CALL IMG_1DGCB(STATUS)

*  set plotting style
        CALL GCB_SETL('ERR_FLAG',.TRUE.,STATUS)
        CALL GCB_SETL('STEP_FLAG',.FALSE.,STATUS)
        CALL GCB_SETL('POLY_FLAG',.FALSE.,STATUS)
        CALL GCB_SETL('POINT_FLAG',.FALSE.,STATUS)

        CALL GCB_CACHE(I_CACHE_1D,STATUS)

        IF (PLOT) THEN
          CALL GDV_CLEAR(STATUS)
          CALL IMG_PLOT(STATUS)

*  flag current plotting status
          I_DISP_1D=.TRUE.
          I_DISP=.FALSE.
          I_CLEAR=.FALSE.
        ELSE
*  if not plotting then resynchronise GCBs
          CALL IMG_2DGCB(STATUS)
        ENDIF


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
	    RNN=1./COUNT(J)/I_XSCALE**2
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
