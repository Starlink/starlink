*+  IAZIMUTH - produces azimuthal distribution
      SUBROUTINE IAZIMUTH(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      7 Aug 90: keyboard mode (RJV)
*      5 Dec 91: V1.2-2 default for cursor mode (RJV)
*     12 Feb 92: V1.2-2 Data units now cnts/square axis unit (RDS)
*     25 Jan 93: V1.7-0 GCB,GFX etc used (RJV)
*      1 Jul 93: V1.7-1 GTR used (RJV)
*     16 Aug 93: V1.7-2 Check status after USI_GETs to stop crash (DJA)
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
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
      REAL TWOPI,DTOR
      PARAMETER (TWOPI=6.28318530717957)
      PARAMETER (DTOR=TWOPI/360.0)
*    Local variables :
      CHARACTER*1 CH
      INTEGER WPNTR
      REAL XC,YC,PXC,PYC
      REAL XR,YR,PXR,PYR,RAD,PRAD
      REAL X,Y,ANG,XSIGN,YSIGN
      INTEGER NBIN
      INTEGER I
      DOUBLE PRECISION UNITFACT          ! Conversion factor between
*                                        ! pixel and square axis unit
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IAZIMUTH Version 1.7-2')
*-
      CALL MSG_PRNT(VERSION)

      CALL USI_INIT()

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  ensure transformations are correct
        CALL GTR_RESTORE(STATUS)
        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

*  cursor mode
        IF (I_MODE.EQ.1) THEN
*  get centre
          CALL MSG_PRNT(' ')
          XC=I_X
          YC=I_Y
          CALL MSG_SETR('X',XC)
          CALL MSG_SETR('Y',YC)
          CALL MSG_PRNT('Select centre/^X,^Y/...')
          CALL PGCURSE(XC,YC,CH)
          IF (CH.EQ.CHAR(13).OR.CH.EQ.CHAR(50)) THEN
            XC=I_X
            YC=I_Y
          ENDIF
          CALL PGPOINT(1,XC,YC,2)
          CALL IMG_WORLDTOPIX(XC,YC,PXC,PYC,STATUS)

*  get radius
          CALL MSG_SETR('R',I_R)
          CALL MSG_PRNT('Select radius/^R/...')
          XR=XC
          YR=YC
          CALL PGCURSE(XR,YR,CH)
          IF (CH.EQ.CHAR(13).OR.CH.EQ.CHAR(50)) THEN
            RAD=I_R
            XR=XC+RAD
            YR=YC
          ELSE
            RAD=SQRT((XR-XC)**2 + (YR-YC)**2)
          ENDIF
          CALL IMG_WORLDTOPIX(XR,YR,PXR,PYR,STATUS)

*  keyboard mode
        ELSE
          CALL USI_DEF0R('XCENT',I_X,STATUS)
          CALL USI_GET0R('XCENT',XC,STATUS)
          CALL USI_DEF0R('YCENT',I_Y,STATUS)
          CALL USI_GET0R('YCENT',YC,STATUS)
          CALL IMG_WORLDTOPIX(XC,YC,PXC,PYC,STATUS)
          CALL USI_GET0R('RAD',RAD,STATUS)
          XR=XC+RAD
          YR=YC
          CALL IMG_WORLDTOPIX(XR,YR,PXR,PYR,STATUS)

        ENDIF

        PRAD=SQRT((PXR-PXC)**2 + (PYR-PYC)**2)
        NBIN=INT(PRAD+0.5)
*  store current position
        I_X=XC
        I_Y=YC
        I_R=RAD

*  plot circle
        CALL IMG_CIRCLE(XC,YC,RAD,STATUS)

*  get number of bins from user
        CALL USI_GET0I('NBIN',NBIN,STATUS)
        IF (STATUS.NE.SAI__OK) GOTO 99

*  get arrays for 1D data
        I_N_1D=NBIN
        CALL IMG_GET1D(NBIN,STATUS)

*  calculate the conversion factor between a square pixel and a square
*  axis unit
        UNITFACT = ABS( I_XSCALE * I_YSCALE )
*
*  Set units to be per whatever the axis units were squared
        I_UNITS_1D = I_UNITS(1:CHR_LEN(I_UNITS)) // ' \ (' //
     &        I_XYUNITS(1:CHR_LEN(I_XYUNITS)) //' squared)'
*
        CALL MSG_SETC('UNIT', I_UNITS_1D)
        CALL MSG_PRNT('Output data will be in ^UNIT')
*
*  set labels
        I_XLABEL_1D='Azimuth'
        I_XUNITS_1D='Degrees'
        I_LABEL_1D='Surface brightness'
        I_TITLE_1D='Azimuthal distribution'

*  set axis values
        I_XWID_1D=360.0/REAL(NBIN)
        I_XBASE_1D=I_XWID_1D*0.5
        I_XSCALE_1D=I_XWID_1D
        CALL ARR_REG1R(I_XBASE_1D,I_XSCALE_1D,I_N_1D,%VAL(I_APTR_1D),
     :                                                        STATUS)
        CALL ARR_INIT1R(ABS(I_XSCALE_1D),I_N_1D,%VAL(I_WPTR_1D),STATUS)

*  show sectors on image
        XSIGN=I_XSCALE/ABS(I_XSCALE)
        YSIGN=I_YSCALE/ABS(I_YSCALE)
        DO I=1,NBIN
          ANG=REAL(I-1)*I_XWID_1D*DTOR
          X=XC+RAD*COS(ANG)*XSIGN
          Y=YC+RAD*SIN(ANG)*YSIGN
          CALL PGMOVE(XC,YC)
          CALL PGDRAW(X,Y)
        ENDDO
        CALL PGPTEXT(XC+RAD*XSIGN,YC,0.0,-0.1,'0')
        CALL PGPTEXT(XC,YC+RAD*YSIGN,90.0,-0.1,'90')
        CALL PGPTEXT(XC-RAD*XSIGN,YC,0.0,1.1,'180')
        CALL PGPTEXT(XC,YC-RAD*YSIGN,270.0,-0.1,'270')

* Map a temporary workspace array
        CALL DYN_MAPI(1,I_N_1D,WPNTR,STATUS)

        CALL IAZIMUTH_DOIT(%VAL(I_DPTR),%VAL(I_VPTR),%VAL(I_QPTR),
     :            UNITFACT,PXC,PYC,PRAD,%VAL(WPNTR),%VAL(I_DPTR_1D),
     :            %VAL(I_VPTR_1D),%VAL(I_QPTR_1D),STATUS)

        CALL DYN_UNMAP(WPNTR,STATUS)

*  reset auxiliary plot
        I_N_AUX=0

*  set default plotting style
        CALL IMG_1DGCB(STATUS)
        CALL GCB_SETL('ERR_FLAG',.TRUE.,STATUS)
        CALL GCB_SETL('STEP_FLAG',.FALSE.,STATUS)
        CALL GCB_SETL('POLY_FLAG',.FALSE.,STATUS)
        CALL GCB_SETL('POINT_FLAG',.FALSE.,STATUS)

*  set default axis ranges
        I_X1_1D=0.0
        I_X2_1D=360.0
        CALL ARR_RANG1R(I_N_1D,%VAL(I_DPTR_1D),I_Y1_1D,I_Y2_1D,STATUS)
        I_Y2_1D=I_Y2_1D+0.1*(I_Y2_1D-I_Y1_1D)

*  go to new zone and plot
        CALL GDV_CLEAR(STATUS)
        CALL IMG_PLOT(STATUS)

*  flag current plotting status
        I_DISP=.FALSE.
        I_DISP_1D=.TRUE.
        I_CLEAR=.FALSE.

 99     CONTINUE

      ENDIF

      CALL USI_CLOSE()

      END



	SUBROUTINE IAZIMUTH_DOIT(ARRAY,VAR,QUAL,UNITFACT,POLX,POLY,
     :                              PRAD,COUNT,POL,PVAR,PQUAL,STATUS)
*    Description :
*    History :
*     Author Dick Willingale 1988-Oct-26
*     Modified for ISIS    Richard Saxton   Mar 30 1989
*     Imported into ASTERIX  9 May 90 RJV
*     Data units now cnts/square axis unit   RDS   Feb 1992
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL ARRAY(I_NX,I_NY)              ! Input cartesian data array
      REAL VAR(I_NX,I_NY)                ! Input variance array
      BYTE QUAL(I_NX,I_NY)               ! Input quality
      REAL POLX,POLY                     ! Centre of polar
      REAL PRAD                          ! radius of polar
      INTEGER COUNT(I_N_1D)              ! Workspace used as a counter
      DOUBLE PRECISION UNITFACT          ! Conversion factor between
*                                        ! pixel and square axis unit
*    Import-Export :
      REAL POL(I_N_1D)                   ! Polar output data array
      REAL PVAR(I_N_1D)                  ! Output variance array
      BYTE PQUAL(I_N_1D)                 ! Output quality
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local constants :
      REAL PI,TWOPI,DTOR
      PARAMETER (TWOPI=6.28318530717957,PI=TWOPI/2.0)
      PARAMETER (DTOR=TWOPI/360.0)
*    Local variables :
      INTEGER NBIN
      INTEGER IXP,IYP,IRAD
      INTEGER NXL,NXH,NYL,NYH                   !Range of pixels to test
      INTEGER J,K,JJ
      REAL YOLD,XOLD,YNEW,XNEW
      REAL RAD                                  !Radius of pixel from centre
      REAL THETA
      DOUBLE PRECISION RNN,RNN2                 !Normalising factors
      REAL RWID
      LOGICAL GOOD
*-
* Code:
      IF (STATUS.NE.SAI__OK) RETURN

      RWID=I_XWID_1D*DTOR
      NBIN=I_N_1D
*
*
* Zero output arrays and counter
       DO K=1,NBIN
	      POL(K)=0.0
	      PVAR(K)=0.0
              COUNT(K)=0
	ENDDO
*
* Find pixel ranges about centre
	IXP=POLX+1.0
	IYP=POLY+1.0
	IRAD=PRAD+2.0
	NXL=MAX(IXP-IRAD,1)
	NXH=MAX(IXP+IRAD,1)
	NYL=MAX(IYP-IRAD,1)
	NYH=MAX(IYP+IRAD,1)
*
* Now raster through array
	DO J=NYL,NYH
	  YOLD=(REAL(J)-0.5)-POLY
	  DO K=NXL,NXH
	    XOLD=(REAL(K)-0.5)-POLX
* Calculate radius of element
	    RAD=XOLD**2+YOLD**2
	    IF(RAD.GT.0.) RAD=SQRT(RAD)
* check pixel is within circle
	    IF(RAD.LE.PRAD) THEN
* Find position angle of element
	      THETA=ATAN2(YOLD,XOLD)
* Force position angle to be in range 0 to TWOPI
	      IF(THETA.LT.0.) THEN
		THETA=TWOPI+THETA
	      ENDIF
	      IF(THETA.LT.0.) THEN
		THETA=TWOPI+THETA
	      ENDIF
	      IF(THETA.GT.TWOPI) THEN
		THETA=THETA-TWOPI
	      ENDIF
	      IF(THETA.GT.TWOPI) THEN
		THETA=THETA-TWOPI
	      ENDIF

*  estimate point in distribution
              JJ=INT(THETA/RWID)+1

              IF (I_QOK.AND.I_BAD) THEN
                GOOD=(BIT_ANDUB(QUAL(K,J),I_MASK).EQ.QUAL__GOOD)
              ELSE
                GOOD=.TRUE.
              ENDIF

              IF (GOOD) THEN
		POL(JJ)=POL(JJ)+ARRAY(K,J)
                COUNT(JJ)=COUNT(JJ)+1
                IF (I_VOK) THEN
		  PVAR(JJ)=PVAR(JJ)+VAR(K,J)
                ENDIF
              ENDIF
            ENDIF
	  ENDDO
        ENDDO
*
* Normalise to surface brightness per pixel
	DO K=1,NBIN

	  IF(COUNT(K).LE.0) THEN
* Mark gaps
            PQUAL(K)=QUAL__MISSING
	  ELSE

            PQUAL(K)=QUAL__GOOD

* Normalise and find standard deviations
	    RNN=1. / (REAL(COUNT(K)) * UNITFACT)
            RNN2=RNN*RNN

	    IF(.NOT.I_VOK) THEN
	      PVAR(K)=POL(K)*RNN2
	    ELSE
	      PVAR(K)=PVAR(K)*RNN2
	    ENDIF

	    POL(K)=POL(K)*RNN

	  ENDIF

	ENDDO

	END
