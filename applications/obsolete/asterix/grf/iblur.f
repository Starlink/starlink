*+  IBLUR - blurs an image by smoothing with box or gaussian filter
      SUBROUTINE IBLUR(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::GKS
*    History :
*     12-Jun-86: Original
*     10-Oct-86: Change parameter names for ADAM (GKS)
*     15-Oct-86: Arrange to get by if pixel sizes not specified (GKS)
*     06-Aug-87: Add gaussian filter (GKS)
*     06-May-88: Arrange to accept data which is not of type IMAGE (GKS)
*     23-May-90: Imported into ASTERIX (RJV)
*      9-Jul-92: Correction to variance handling (RJV)
*      4-Aug-92: Handles data with bad quality (sets to zero) (RJV)
*     13 Sep 94: V1.7-1 updates data min/max (RJV)
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
      REAL XFWHM,YFWHM
      REAL XBOX,YBOX
      LOGICAL GAUSSIAN
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IBLUR Version 1.7-1')
*-
      CALL MSG_PRNT(VERSION)

      CALL USI_INIT()

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSE


        CALL USI_GET0L('GAUSS',GAUSSIAN,STATUS)

	IF(GAUSSIAN) THEN

           CALL USI_PROMT('XWID','Gaussian FWHM in X (pixels)', STATUS)
           CALL USI_GET0R('XWID',XFWHM,STATUS)
           CALL USI_PROMT('YWID','Gaussian FWHM in Y (pixels)', STATUS)
           CALL USI_GET0R('YWID',YFWHM,STATUS)

	ELSE

           CALL USI_PROMT('XWID','Box size in X (pixels)', STATUS)
           CALL USI_GET0R('XWID',XBOX,STATUS)
           CALL USI_PROMT('YWID','Box size in Y (pixels)', STATUS)
           CALL USI_GET0R('YWID',YBOX,STATUS)

	ENDIF

        CALL IMG_COPY(.FALSE.,.FALSE.,STATUS)
        CALL IMG_COPYBITS(STATUS)
        I_TITLE_W='Smoothed image'
        I_CAN_UNDO=.FALSE.

        IF (I_BAD) THEN
          CALL MSG_PRNT('Warning: data contains bad pixels - '//
     :                        'will be treated as zero')
        ENDIF
*  do smoothing
	IF (GAUSSIAN) THEN
           IF (I_BAD) THEN
	     CALL IBLUR_GQ(%VAL(I_DPTR),%VAL(I_VPTR),%VAL(I_QPTR),
     :                   XFWHM,YFWHM,%VAL(I_DPTR_W),%VAL(I_VPTR_W),
     :                                                       STATUS)
           ELSE
	     CALL IBLUR_G(%VAL(I_DPTR),%VAL(I_VPTR),XFWHM,YFWHM,
     :                       %VAL(I_DPTR_W),%VAL(I_VPTR_W),STATUS)
           ENDIF
	ELSE
           IF (I_BAD) THEN
	     CALL IBLUR_BQ(%VAL(I_DPTR),%VAL(I_VPTR),%VAL(I_QPTR),
     :                       XBOX,YBOX,%VAL(I_DPTR_W),%VAL(I_VPTR_W),
     :                                                         STATUS)
           ELSE
	     CALL IBLUR_B(%VAL(I_DPTR),%VAL(I_VPTR),XBOX,YBOX,
     :                       %VAL(I_DPTR_W),%VAL(I_VPTR_W),STATUS)
           ENDIF
	ENDIF

        IF (STATUS.EQ.SAI__OK) THEN

*  copy quality to work buffer
          IF (I_QOK) THEN
            CALL ARR_COP1B(I_NX*I_NY,%VAL(I_QPTR),%VAL(I_QPTR_W),
     :                                                     STATUS)
          ENDIF
          CALL IMG_SWAP(STATUS)
          CALL IMG_MINMAX(STATUS)
          I_PROC_COUNT=I_PROC_COUNT+1
          I_CAN_UNDO=.TRUE.
          I_LAST_CMD='IBLUR'

        ENDIF


      ENDIF

      CALL USI_CLOSE()

      END


      SUBROUTINE IBLUR_B(IDATA,IVAR,XBOX,YBOX,ODATA,OVAR,STATUS)
*    Description :
*     applies a box filter function; NB errors are averaged in same way
*    Method :
*      There is no wrap-around: at edges averaging is over reduced number of
*      elements.
*      Box has 'soft' edges to allow for sizes other than an odd integral
*      number of pixels. Weight depends on overlap of box and square pixel.
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::GKS
*    History :
*     13-Jun-86
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
	REAL IDATA(I_NX,I_NY)			! input data
	REAL IVAR(I_NX,I_NY)			! input error
	REAL XBOX,YBOX				! box size, in pixels
*    Export :
	REAL ODATA(I_NX,I_NY)			! output data
	REAL OVAR(I_NX,I_NY)			! output error
*    Status :
        INTEGER STATUS
*    Local Constants :

*    Local variables :
	INTEGER I,J				! do loop variables
	INTEGER I1,J1				! ditto
	INTEGER IMIN,IMAX			! limits, x direction
	INTEGER JMIN,JMAX			! limits, y direction
	REAL SUMD,SUMV				! sum of data and variance
	REAL SUMW				! sum of weights
	REAL WX,WY,WXY				! weights (x,y components; nett)
	REAL HBX,HBY				! half side of an inner box
						! ouside which there is only
						! a partial contribution
	INTEGER IHBX,IHBY			! integer versions of above


*-
      IF (STATUS.NE.SAI__OK) RETURN

        CALL MSG_PRNT(' ')
        CALL MSG_PRNT('Smoothing...')

	HBX=(XBOX/2.0)
	IHBX=NINT(HBX)
	HBY=(YBOX/2.0)
	IHBY=NINT(HBY)
	DO J=1,I_NY
	   JMIN=MAX0(1,(J-IHBY))
	   JMAX=MIN0(I_NY,(J+IHBY))
	   DO I=1,I_NX
	      IMIN=MAX0(1,(I-IHBX))
	      IMAX=MIN0(I_NX,(I+IHBX))
	      SUMD=0.0
	      SUMV=0.0
	      SUMW=0.0
	      DO J1=JMIN,JMAX
	         WY=AMIN1(1.0,HBY+0.5-ABS(J-J1))
	         DO I1=IMIN,IMAX
	            WX=AMIN1(1.0,HBX+0.5-ABS(I-I1))
	            WXY=WX*WY
	            SUMD=SUMD+IDATA(I1,J1)*WXY
	            IF(I_VOK) THEN
                      SUMV=SUMV+IVAR(I1,J1)*WXY**2
                    ENDIF
	            SUMW=SUMW+WXY
	         END DO
	      END DO
	      ODATA(I,J)=SUMD/SUMW
	      IF(I_VOK) THEN
                OVAR(I,J)=SUMV/SUMW**2
              ENDIF
	   END DO
	END DO

	END



      SUBROUTINE IBLUR_G(IDATA,IVAR,XFWHM,YFWHM,ODATA,OVAR,STATUS)
*    Description :
*     applies a Gaussian filter function; NB errors are averaged in same way
*    Method :
*      There is no wrap-around: at edges averaging is over reduced number of
*      elements.
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::GKS
*    History :
*     13-Jun-86
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
	REAL IDATA(I_NX,I_NY)			! input data
	REAL IVAR(I_NX,I_NY)			! input error
	REAL XFWHM,YFWHM			! FWHM of Gaussian in pixels
*    Export :
	REAL ODATA(I_NX,I_NY)			! output data
	REAL OVAR(I_NX,I_NY)			! output error
*    Status :
        INTEGER STATUS
*    Local Constants :
	INTEGER ARRAYSIZ			! (half) size of arrays for
	 PARAMETER (ARRAYSIZ=200)		!   storing Gaussian functions

*    Local variables :
	INTEGER I,J				! do loop variables
	INTEGER I1,J1				! ditto
	INTEGER IMIN,IMAX			! limits, x direction
	INTEGER JMIN,JMAX			! limits, y direction
	REAL X0,Y0				! 1/e points
	REAL SUMD,SUMV				! sum of data and error
	REAL SUMW				! sum of weights
	REAL WX,WY,WXY				! weights (x,y components; nett)
	REAL HBX,HBY				! half side of box over which
						! Gaussian is considered
						! non-zero
	INTEGER IHBX,IHBY			! integer versions of above
	REAL XFILT(-ARRAYSIZ:ARRAYSIZ)
	REAL YFILT(-ARRAYSIZ:ARRAYSIZ)

*-
* entry:-

      IF (STATUS.NE.SAI__OK) RETURN

	X0=XFWHM/(2.0*SQRT(ALOG(2.0)))
	Y0=YFWHM/(2.0*SQRT(ALOG(2.0)))

	HBX=(X0*2.5)
	IHBX=NINT(HBX)
	HBY=(Y0*2.5)
	IHBY=NINT(HBY)
	IF ((IHBX.GT.ARRAYSIZ).OR.(IHBY.GT.ARRAYSIZ)) THEN
           CALL MSG_PRNT('AST_ERR: filter too big')
           STATUS=SAI__ERROR

        ELSE

          CALL MSG_PRNT(' ')
          CALL MSG_PRNT('Smoothing...')


* calculate the x,y parts of the filter function
	  DO I=0,IHBX
	     XFILT(I)=EXP(-(REAL(I)/X0)**2)
	     XFILT(-I)=XFILT(I)
	  END DO
	  DO I=0,IHBY
	     YFILT(I)=EXP(-(REAL(I)/Y0)**2)
	     YFILT(-I)=YFILT(I)
	  END DO

* do the job
	  DO J=1,I_NY
	     JMIN=MAX0(1,(J-IHBY))
	     JMAX=MIN0(I_NY,(J+IHBY))
	     DO I=1,I_NX
	        IMIN=MAX0(1,(I-IHBX))
	        IMAX=MIN0(I_NX,(I+IHBX))
	        SUMD=0.0
	        SUMV=0.0
	        SUMW=0.0
	        DO J1=JMIN,JMAX
	           WY=YFILT(J-J1)
	           DO I1=IMIN,IMAX
	              WXY=XFILT(I-I1)*WY
	              SUMD=SUMD+IDATA(I1,J1)*WXY
	              IF(I_VOK) THEN
                        SUMV=SUMV+IVAR(I1,J1)*WXY**2
                      ENDIF
	              SUMW=SUMW+WXY
	           END DO
	        END DO
	        ODATA(I,J)=SUMD/SUMW
	        IF(I_VOK) THEN
                  OVAR(I,J)=SUMV/SUMW**2
                ENDIF
	     END DO
	  END DO

        ENDIF

	END



      SUBROUTINE IBLUR_BQ(IDATA,IVAR,IQ,XBOX,YBOX,ODATA,OVAR,STATUS)
*    Description :
*     applies a box filter function; NB errors are averaged in same way
*    Method :
*      There is no wrap-around: at edges averaging is over reduced number of
*      elements.
*      Box has 'soft' edges to allow for sizes other than an odd integral
*      number of pixels. Weight depends on overlap of box and square pixel.
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::GKS
*    History :
*     13-Jun-86
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
	REAL IDATA(I_NX,I_NY)			! input data
	REAL IVAR(I_NX,I_NY)			! input error
        BYTE IQ(I_NX,I_NY)			! input quality
	REAL XBOX,YBOX				! box size, in pixels
*    Export :
	REAL ODATA(I_NX,I_NY)			! output data
	REAL OVAR(I_NX,I_NY)			! output error
*    Status :
        INTEGER STATUS
*    Functions :
        BYTE BIT_ANDUB
*    Local Constants :

*    Local variables :
	INTEGER I,J				! do loop variables
	INTEGER I1,J1				! ditto
	INTEGER IMIN,IMAX			! limits, x direction
	INTEGER JMIN,JMAX			! limits, y direction
	REAL SUMD,SUMV				! sum of data and variance
	REAL SUMW				! sum of weights
	REAL WX,WY,WXY				! weights (x,y components; nett)
	REAL HBX,HBY				! half side of an inner box
						! ouside which there is only
						! a partial contribution
	INTEGER IHBX,IHBY			! integer versions of above


*-
      IF (STATUS.NE.SAI__OK) RETURN

        CALL MSG_PRNT(' ')
        CALL MSG_PRNT('Smoothing...')

	HBX=(XBOX/2.0)
	IHBX=NINT(HBX)
	HBY=(YBOX/2.0)
	IHBY=NINT(HBY)
	DO J=1,I_NY
	   JMIN=MAX0(1,(J-IHBY))
	   JMAX=MIN0(I_NY,(J+IHBY))
	   DO I=1,I_NX
	      IMIN=MAX0(1,(I-IHBX))
	      IMAX=MIN0(I_NX,(I+IHBX))
	      SUMD=0.0
	      SUMV=0.0
	      SUMW=0.0
	      DO J1=JMIN,JMAX
	         WY=AMIN1(1.0,HBY+0.5-ABS(J-J1))
	         DO I1=IMIN,IMAX
	            WX=AMIN1(1.0,HBX+0.5-ABS(I-I1))
	            WXY=WX*WY
                    IF (BIT_ANDUB(IQ(I1,J1),I_MASK).EQ.QUAL__GOOD) THEN
	              SUMD=SUMD+IDATA(I1,J1)*WXY
	              IF(I_VOK) THEN
                        SUMV=SUMV+IVAR(I1,J1)*WXY**2
                      ENDIF
                    ENDIF
	            SUMW=SUMW+WXY
	         END DO
	      END DO
	      ODATA(I,J)=SUMD/SUMW
	      IF(I_VOK) THEN
                OVAR(I,J)=SUMV/SUMW**2
              ENDIF
	   END DO
	END DO

	END



      SUBROUTINE IBLUR_GQ(IDATA,IVAR,IQ,XFWHM,YFWHM,ODATA,OVAR,STATUS)
*    Description :
*     applies a Gaussian filter function; NB errors are averaged in same way
*    Method :
*      There is no wrap-around: at edges averaging is over reduced number of
*      elements.
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::GKS
*    History :
*     13-Jun-86
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
	REAL IDATA(I_NX,I_NY)			! input data
	REAL IVAR(I_NX,I_NY)			! input error
        BYTE IQ(I_NX,I_NY)			! input quality
	REAL XFWHM,YFWHM			! FWHM of Gaussian in pixels
*    Export :
	REAL ODATA(I_NX,I_NY)			! output data
	REAL OVAR(I_NX,I_NY)			! output error
*    Status :
        INTEGER STATUS
*    Functions :
        BYTE BIT_ANDUB
*    Local Constants :
	INTEGER ARRAYSIZ			! (half) size of arrays for
	 PARAMETER (ARRAYSIZ=200)		!   storing Gaussian functions

*    Local variables :
	INTEGER I,J				! do loop variables
	INTEGER I1,J1				! ditto
	INTEGER IMIN,IMAX			! limits, x direction
	INTEGER JMIN,JMAX			! limits, y direction
	REAL X0,Y0				! 1/e points
	REAL SUMD,SUMV				! sum of data and error
	REAL SUMW				! sum of weights
	REAL WX,WY,WXY				! weights (x,y components; nett)
	REAL HBX,HBY				! half side of box over which
						! Gaussian is considered
						! non-zero
	INTEGER IHBX,IHBY			! integer versions of above
	REAL XFILT(-ARRAYSIZ:ARRAYSIZ)
	REAL YFILT(-ARRAYSIZ:ARRAYSIZ)

*-

      IF (STATUS.NE.SAI__OK) RETURN

	X0=XFWHM/(2.0*SQRT(ALOG(2.0)))
	Y0=YFWHM/(2.0*SQRT(ALOG(2.0)))

	HBX=(X0*2.5)
	IHBX=NINT(HBX)
	HBY=(Y0*2.5)
	IHBY=NINT(HBY)
	IF ((IHBX.GT.ARRAYSIZ).OR.(IHBY.GT.ARRAYSIZ)) THEN
           CALL MSG_PRNT('AST_ERR: filter too big')
           STATUS=SAI__ERROR

        ELSE

          CALL MSG_PRNT(' ')
          CALL MSG_PRNT('Smoothing...')


* calculate the x,y parts of the filter function
	  DO I=0,IHBX
	     XFILT(I)=EXP(-(REAL(I)/X0)**2)
	     XFILT(-I)=XFILT(I)
	  END DO
	  DO I=0,IHBY
	     YFILT(I)=EXP(-(REAL(I)/Y0)**2)
	     YFILT(-I)=YFILT(I)
	  END DO

* do the job
	  DO J=1,I_NY
	     JMIN=MAX0(1,(J-IHBY))
	     JMAX=MIN0(I_NY,(J+IHBY))
	     DO I=1,I_NX
	        IMIN=MAX0(1,(I-IHBX))
	        IMAX=MIN0(I_NX,(I+IHBX))
	        SUMD=0.0
	        SUMV=0.0
	        SUMW=0.0
	        DO J1=JMIN,JMAX
	           WY=YFILT(J-J1)
	           DO I1=IMIN,IMAX
	              WXY=XFILT(I-I1)*WY
                      IF (BIT_ANDUB(IQ(I1,J1),I_MASK)
     :                                     .EQ.QUAL__GOOD) THEN
	                SUMD=SUMD+IDATA(I1,J1)*WXY
	                IF(I_VOK) THEN
                          SUMV=SUMV+IVAR(I1,J1)*WXY**2
                        ENDIF
                      ENDIF
	              SUMW=SUMW+WXY
	           END DO
	        END DO
	        ODATA(I,J)=SUMD/SUMW
	        IF(I_VOK) THEN
                  OVAR(I,J)=SUMV/SUMW**2
                ENDIF
	     END DO
	  END DO

        ENDIF

	END
