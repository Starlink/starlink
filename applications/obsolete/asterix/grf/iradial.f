*+  IRADIAL - produces radial plot
      SUBROUTINE IRADIAL(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*       5 Dec 91 : V1.2-1 Default for cursor mode (RJV)
*      13 Feb 92 : V1.5-0 Added oversampling and output plot now in axis
*                         units (DJA)
*      26 Jan 93 : V1.7-0 GCB, GFX used (RJV)
*       1 Jul 93 : V1.7-1 GTR used (RJV)
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
*    Local variables :
      CHARACTER*1 CH
      INTEGER WPNTR
      REAL XC,YC,PXC,PYC
      REAL XR,YR,PXR,PYR,RAD,PRAD
      INTEGER NBIN
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IRADIAL Version 1.7-1')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  ensure transformations correct
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
          XR=XC
          YR=YC
          CALL MSG_SETR('R',I_R)
          CALL MSG_PRNT('Select radius/^R/...')
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
          CALL PAR_DEF0R('XCENT',I_X,STATUS)
          CALL PAR_GET0R('XCENT',XC,STATUS)
          CALL PAR_DEF0R('YCENT',I_Y,STATUS)
          CALL PAR_GET0R('YCENT',YC,STATUS)
          CALL IMG_WORLDTOPIX(XC,YC,PXC,PYC,STATUS)
          CALL PAR_GET0R('RAD',RAD,STATUS)
          XR=XC+RAD
          YR=YC
          CALL IMG_WORLDTOPIX(XR,YR,PXR,PYR,STATUS)

        ENDIF

*  get oversample
        CALL PAR_GET0I( 'SAMPLE', I_OSAMPLE, STATUS )

        PRAD=SQRT((PXR-PXC)**2 + (PYR-PYC)**2)
        NBIN=I_OSAMPLE*INT(PRAD+0.5)
        IF ( NBIN .LE. 0 ) THEN
          CALL MSG_PRNT( '! Circle too small' )
          GOTO 99
        END IF

*  store current position
        I_X=XC
        I_Y=YC
        I_R=RAD

*  plot circle
        CALL IMG_CIRCLE(XC,YC,RAD,STATUS)

*  get arrays for 1D data
        I_N_1D=NBIN
        CALL IMG_GET1D(NBIN,STATUS)

*  set labels
        I_XLABEL_1D='Radius'
        I_XUNITS_1D=I_XYUNITS
        I_LABEL_1D='Surface brightness'
        I_TITLE_1D='Radial distribution'
*
*  Set data units to be per pixel
        I_UNITS_1D = I_UNITS(1:CHR_LEN(I_UNITS)) // ' / pixel'
*
*  set axis values
        I_XBASE_1D=ABS(I_XSCALE)/REAL(I_OSAMPLE)/2.0
        I_XSCALE_1D=ABS(I_XSCALE)/REAL(I_OSAMPLE)
        I_XWID_1D=ABS(I_XSCALE)/REAL(I_OSAMPLE)
        CALL ARR_REG1R(I_XBASE_1D,I_XSCALE_1D,I_N_1D,%VAL(I_APTR_1D),
     :                                                        STATUS)
        CALL ARR_INIT1R(ABS(I_XSCALE_1D),I_N_1D,%VAL(I_WPTR_1D),STATUS)

* Map a temporary workspace array
        CALL DYN_MAPI(1,I_N_1D,WPNTR,STATUS)

        CALL IRADIAL_DOIT(I_OSAMPLE,I_NX,I_NY,%VAL(I_DPTR),
     :                    %VAL(I_VPTR),.TRUE.,
     :                    %VAL(I_QPTR),.TRUE.,
     :                    PXC,PYC,%VAL(WPNTR),%VAL(I_DPTR_1D),
     :                    %VAL(I_VPTR_1D),%VAL(I_QPTR_1D),STATUS)

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
        I_X2_1D=ABS(I_XSCALE)*REAL(I_N_1D)/REAL(I_OSAMPLE)
        CALL ARR_RANG1R(I_N_1D,%VAL(I_DPTR_1D),I_Y1_1D,I_Y2_1D,STATUS)
        I_Y2_1D=I_Y2_1D+0.1*(I_Y2_1D-I_Y1_1D)

*  go to new zone and plot
        CALL GDV_CLEAR(STATUS)
        CALL IMG_PLOT(STATUS)

*  flag current plotting status
        I_DISP=.FALSE.
        I_DISP_1D=.TRUE.
        I_CLEAR=.FALSE.

      ENDIF

 99   CONTINUE

      END



	SUBROUTINE IRADIAL_DOIT(SAMPLE,NX,NY,ARRAY,VAR,USE_VAR,QUAL,
     :                          USE_QUAL, POLX,POLY,
     :                               COUNT,POL,PVAR,PQUAL,STATUS)
*    Description :
*    History :
*     Author Dick Willingale 1988-Oct-26
*     Modified for ISIS    Richard Saxton   Mar 30 1989
*      Imported into ASTERIX  9 May 90 RJV
*     13-Feb-92 : Added oversampling and extra arguments to make this
*                 usable by IPSF too (DJA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      INTEGER SAMPLE                     ! Oversampling
      INTEGER NX,NY                      ! Input array sizes
      REAL ARRAY(NX,NY)                  ! Input cartesian data array
      REAL VAR(NX,NY)                    ! Input variance array
      LOGICAL USE_VAR                    ! Use variance?
      BYTE QUAL(NX,NY)                   ! Input quality
      LOGICAL USE_QUAL                   ! Use quality?
      REAL POLX,POLY                     ! Centre of polar
      INTEGER COUNT(I_N_1D)              ! Workspace used as a counter
*    Import-Export :
      REAL POL(I_N_1D)                   ! Polar output data array
      REAL PVAR(I_N_1D)                  ! Output variance array
      BYTE PQUAL(I_N_1D)                 ! Output quality
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local variables :
      INTEGER NRAD
      INTEGER IXP,IYP,IRAD
      INTEGER NXL,NXH,NYL,NYH                   !Range of pixels to test
      INTEGER J,K,KK,JJ,II
      REAL YOLD,XOLD,YNEW,XNEW
      REAL DFRAC
      REAL RAD                                  !Radius of pixel from centre
      REAL RNN,RNN2                             !Normalising factors
      REAL XP,YP
      LOGICAL GOOD
*-
* Code:
      IF (STATUS.NE.SAI__OK) RETURN

      NRAD=I_N_1D

*    Zero output arrays and counter
      DO K=1,NRAD
	POL(K)=0.0
	IF (USE_VAR) PVAR(K)=0.0
        COUNT(K)=0
      END DO

*    Find pixel ranges about centre
      IXP=POLX+1.0
      IYP=POLY+1.0
      IRAD=INT(REAL(NRAD)/REAL(SAMPLE))+1

      NXL=MAX(IXP-IRAD,1)
      NXH=MIN(IXP+IRAD,NX)
      NYL=MAX(IYP-IRAD,1)
      NYH=MIN(IYP+IRAD,NY)

*    Correction to data values due to oversampling
      DFRAC = 1.0/REAL(SAMPLE**2)

*    Now raster through array
      DO J=NYL,NYH
	YOLD=REAL(J-1) - POLY
        DO K=NXL,NXH
	  XOLD=REAL(K-1) - POLX

*        Good pixel?
          IF (USE_QUAL.AND.I_QOK.AND.I_BAD) THEN
            GOOD=(BIT_ANDUB(QUAL(K,J),I_MASK).EQ.QUAL__GOOD)
          ELSE
            GOOD=.TRUE.
          END IF

          IF (GOOD) THEN

*          Oversample
            DO JJ = 1, SAMPLE
              YP = YOLD + (REAL(JJ)-0.5)/REAL(SAMPLE)
              DO II = 1, SAMPLE
                XP = XOLD + (REAL(II)-0.5)/REAL(SAMPLE)

*              Find radial bin number
                KK = INT(SQRT(XP*XP+YP*YP)*REAL(SAMPLE))+1

*              In valid range?
	        IF(KK.GT.0.AND.KK.LE.NRAD) THEN
                  COUNT(KK)=COUNT(KK)+1
		  POL(KK)=POL(KK)+ARRAY(K,J)
                  IF (USE_VAR.AND.I_VOK) THEN
		    PVAR(KK)=PVAR(KK)+VAR(K,J)
                  END IF
                ENDIF

              END DO
            END DO

          END IF

        END DO
      END DO

*    Normalise to surface brightness per pixel
      DO K=1,NRAD

	  IF(COUNT(K).LE.0) THEN
* Mark gaps
            IF ( USE_QUAL) PQUAL(K)=QUAL__MISSING
	  ELSE

*          Correct bin count for oversampling
            POL(K) = POL(K)*DFRAC

            IF ( USE_QUAL) PQUAL(K)=QUAL__GOOD

* Normalise and find standard deviations
	    RNN=1.0/REAL(COUNT(K))/DFRAC
            RNN2=RNN*RNN

	    IF(USE_VAR.AND.I_VOK) THEN

*            Correct bin variance for oversampling
              PVAR(K) = PVAR(K)*DFRAC*DFRAC

	      PVAR(K)=PVAR(K)*RNN2
	    ELSE IF ( USE_VAR ) THEN

*            Set variance of zero pixels to same as pixels with 1 count
              IF ( POL(K) .EQ. 0.0 ) THEN
	        PVAR(K)=DFRAC*RNN2
              ELSE
	        PVAR(K)=POL(K)*RNN2
              END IF

	    ENDIF

	    POL(K)=POL(K)*RNN

	  ENDIF

	ENDDO

	END
