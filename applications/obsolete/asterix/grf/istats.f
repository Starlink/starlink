*+  ISTATS - gives basic statistics on selected region of image
      SUBROUTINE ISTATS(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*     13 Aug 90: V1.2-1 output values to parameters (RJV)
*     15 Aug 90: V1.2-2 added circular region (RJV)
*     15 Aug 90: V1.2-3 centroid added (RJV)
*     28 Nov 90: V1.2-4 correction to variance calc. (RJV)
*      5 Jun 91: V1.2-5 screen output suppression option (RJV)
*      5 Jul 91: V1.2-6 integer overflow prob. for large images fixed (RJV)
*     20 Jul 91: V1.2-7 corrected error in variance of mean (RJV)
*      4 Dec 91: V1.2-8 improvements to circle bit (RJV)
*     16 Feb 94: V1.2-9 BOX and CIRC options (RJV)
*     23 Feb 94: V1.2-10 Error in mean corrected again (RJV)
*     28 Sep 94: V1.7-0 uses region definitions (RJV)
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
      REAL MAX,MIN
      REAL MEAN,MERR
      REAL TOT
      REAL XC,YC,DX,DY,RAD
      REAL XCENT,YCENT
      INTEGER NGOOD,NBAD
      LOGICAL SUPPRESS
      LOGICAL BOX,CIRC
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ISTATS Version 1.7-0')
*-
*  is screen output required?
      CALL PAR_GET0L('SUPPRESS',SUPPRESS,STATUS)

      IF (.NOT.SUPPRESS) THEN
        CALL MSG_PRNT(VERSION)
      ENDIF


      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSE

        CIRC=.FALSE.
        BOX=.FALSE.
        CALL PAR_GET0L('CIRC',CIRC,STATUS)
        IF (.NOT.CIRC) THEN
          CALL PAR_GET0L('BOX',BOX,STATUS)
        ENDIF

        IF ((BOX.OR.CIRC).AND..NOT.I_DISP) THEN
          CALL MSG_PRNT('AST_ERR: no image currently displayed')
          CALL MSG_PRNT('         will default to currently')
          CALL MSG_PRNT('         selected region')
          BOX=.FALSE.
          CIRC=.FALSE.
        ELSE


          IF (BOX) THEN
            CALL GTR_RESTORE(STATUS)
            CALL IMG_GETBOX('XC','YC','XWID','YWID',XC,YC,DX,DY,STATUS)
            CALL IMG_BOX(XC,YC,DX,DY,STATUS)
          ELSEIF (CIRC) THEN
            CALL GTR_RESTORE(STATUS)
            CALL IMG_GETCIRC('XC','YC','RAD',XC,YC,RAD,STATUS)
            CALL IMG_CIRCLE(XC,YC,RAD,STATUS)
          ENDIF

        ENDIF

*  specified circle
        IF (CIRC) THEN
          CALL ISTATS_CIRC(%VAL(I_DPTR),%VAL(I_VPTR),%VAL(I_QPTR),
     :                   XC,YC,RAD,MIN,MAX,MEAN,TOT,MERR,NGOOD,NBAD,
     :                                           XCENT,YCENT,STATUS)
*  specified box
        ELSEIF (BOX) THEN
          CALL ISTATS_RECT(%VAL(I_DPTR),%VAL(I_VPTR),%VAL(I_QPTR),
     :                 XC,YC,DX,DY,MIN,MAX,MEAN,TOT,MERR,NGOOD,NBAD,
     :                                           XCENT,YCENT,STATUS)
*  current region
        ELSE
          CALL ISTATS_REG(%VAL(I_DPTR),%VAL(I_VPTR),%VAL(I_QPTR),
     :                            MIN,MAX,MEAN,TOT,MERR,NGOOD,NBAD,
     :                                           XCENT,YCENT,STATUS)

        ENDIF


*  display results
        IF (.NOT.SUPPRESS) THEN
          CALL MSG_BLNK()
          IF (CIRC) THEN
            CALL MSG_PRNT(
     :     '   ********** Statistics for selected circle **********')
          ELSEIF (BOX) THEN
            CALL MSG_PRNT(
     :     '   ********** Statistics for selected box **********')
          ELSEIF (I_REG_TYPE.EQ.'NONE') THEN
            CALL MSG_PRNT(
     :     '   ********** Statistics for whole image **********')
          ELSE
            CALL MSG_PRNT(
     :     '   ********** Statistics for current region ***********')
          ENDIF
          CALL MSG_BLNK()
          CALL MSG_SETI('NG',NGOOD)
          CALL MSG_PRNT('      Number of good pixels : ^NG')
          CALL MSG_SETI('NB',NBAD)
          CALL MSG_PRNT('      Number of bad  pixels : ^NB')
          CALL MSG_SETR('MAX',MAX)
          CALL MSG_PRNT('                 Max  value : ^MAX')
          CALL MSG_SETR('MIN',MIN)
          CALL MSG_PRNT('                 Min  value : ^MIN')
          CALL MSG_SETR('TOT',TOT)
          CALL MSG_PRNT('                      Total : ^TOT')
          CALL MSG_SETR('MEAN',MEAN)
          CALL MSG_PRNT('                 Mean value : ^MEAN')
          CALL MSG_SETR('ERR',MERR)
          CALL MSG_PRNT('              Error on mean : ^ERR')
          CALL MSG_SETR('XCEN',XCENT)
          CALL MSG_SETR('YCEN',YCENT)
          CALL MSG_PRNT('                   Centroid : ^XCEN,^YCEN')
          CALL MSG_BLNK()
        ENDIF

*  output to environment
        CALL PAR_PUT0R('MIN',MIN,STATUS)
        CALL PAR_PUT0R('MAX',MAX,STATUS)
        CALL PAR_PUT0R('MEAN',MEAN,STATUS)
        CALL PAR_PUT0R('TOT',TOT,STATUS)
        CALL PAR_PUT0I('NPIX',NGOOD,STATUS)
        CALL PAR_PUT0R('XCENT',XCENT,STATUS)
        CALL PAR_PUT0R('YCENT',YCENT,STATUS)

      ENDIF

      END

*+ ISTATS_RECT      Integrates the pixels in a subset of a 2-d data array
        SUBROUTINE ISTATS_RECT(ARRAY,VAR,QUAL,XC,YC,DX,DY,
     :                               DMIN,DMAX,MEAN,TOTAL,MERR,
     :                               NGOOD,NBAD,XCENT,YCENT,STATUS)
*    Description :
*        Finds the mean pixel value from a rectangular subset of a 2-d array.
*        If a variance array exists in the input file, the output varaince
*        is calculated as the mean of the pixel variances, if no variance array
*        exists then variance is calculated as:
*           VAR = sum(VAL**2)/NPIXELS - [sum(VAL)/NPIXELS]**2
*
*       If quality information is available then each pixel is checked and
*       bad ones are excluded from the stats calculation.
*    History :
*     9-May-1989 original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL ARRAY(I_NX,I_NY)                ! Data array
      REAL VAR(I_NX,I_NY)                  ! Variance array
      BYTE QUAL(I_NX,I_NY)                 ! Quality array
      REAL XC,YC,DX,DY
*    Import-Export :
*    Export :
      REAL DMIN
      REAL DMAX
      REAL MEAN                            ! Mean value of pixels found in
*                                          !   subset of array
      REAL TOTAL
      REAL MERR                            ! Error on the above mean
      INTEGER NGOOD                        ! Number of good pixels
      INTEGER NBAD                         ! Number of bad pixels
      REAL XCENT,YCENT
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
*    Local variables :
      REAL VALVAR			   ! value weighted by variance
      REAL XPIX,YPIX
      INTEGER IX,IY
      INTEGER I1,I2,J1,J2
      LOGICAL GOOD
*-
      IF (STATUS.NE.SAI__OK) RETURN

* Initialise:
      TOTAL=0.0
      MERR=0.0
      NGOOD=0
      NBAD=0
      DMIN=I_DMAX
      DMAX=I_DMIN
      XPIX=0.0
      YPIX=0.0

* get range of pixels for this box
      CALL IMG_BOXTOBOX(XC,YC,DX,DY,I1,I2,J1,J2,STATUS)
*
* Loop over all pixels
      DO IY=J1,J2
        DO IX=I1,I2

*   Test pixel quality
            IF (I_BAD) THEN
              GOOD=(BIT_ANDUB(QUAL(IX,IY),I_MASK).EQ.QUAL__GOOD)
            ELSE
              GOOD=.TRUE.
            ENDIF

            IF (GOOD) THEN
*     Sum the pixel values
               TOTAL=TOTAL+ARRAY(IX,IY)
               XPIX=XPIX+REAL(IX)*ARRAY(IX,IY)
               YPIX=YPIX+REAL(IY)*ARRAY(IX,IY)
               DMIN=MIN(ARRAY(IX,IY),DMIN)
               DMAX=MAX(ARRAY(IX,IY),DMAX)

               NGOOD=NGOOD+1
*
            ELSE
*
               NBAD=NBAD+1
*
            ENDIF

*
         ENDDO
*
      ENDDO
*
* Normalise
      IF (NGOOD .GT. 0) THEN
*
         MEAN=TOTAL/REAL(NGOOD)
         XPIX=XPIX/TOTAL
         YPIX=YPIX/TOTAL
         CALL IMG_PIXTOWORLD(XPIX,YPIX,XCENT,YCENT,STATUS)
* get error on mean
         CALL ISTATS_RECT_MEANVAR(I1,I2,J1,J2,ARRAY,QUAL,MEAN,MERR,
     :                                                      STATUS)
*
      ENDIF
*
*
      END
*


        SUBROUTINE ISTATS_RECT_MEANVAR(I1,I2,J1,J2,ARRAY,QUAL,MEAN,MERR,
     :                                                           STATUS)

*    Description :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL MEAN                            ! Mean value of pixels found in
      REAL ARRAY(I_NX,I_NY)                ! Data array
      BYTE QUAL(I_NX,I_NY)                 ! Quality array
      INTEGER I1,I2,J1,J2
*    Import-Export :
*    Export :
*                                          !   subset of array
      REAL MERR                            ! Error on the above mean
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
*    Local variables :
      REAL RGOOD
      INTEGER IX,IY
      INTEGER NGOOD
      LOGICAL GOOD

*-
      IF (STATUS.NE.SAI__OK) RETURN

* Initialise:
      MERR=0.0
      NGOOD=0
*
*
* Loop over all pixels
      DO IY=J1,J2
        DO IX=I1,I2

*   Test pixel quality
            IF (I_BAD) THEN
              GOOD=(BIT_ANDUB(QUAL(IX,IY),I_MASK).EQ.QUAL__GOOD)
            ELSE
              GOOD=.TRUE.
            ENDIF

            IF (GOOD) THEN
               MERR=MERR+(ARRAY(IX,IY)-MEAN)**2
*
               NGOOD=NGOOD+1
*
            ENDIF

*
         ENDDO
*
      ENDDO
*
      IF (NGOOD .GT. 1) THEN
        MERR=SQRT(MERR)
        RGOOD=REAL(NGOOD)
        MERR=MERR/SQRT(RGOOD)/SQRT(RGOOD-1.0)

      ENDIF
*
*
      END
*



*+ ISTATS_CIRC      Integrates the pixels in a subset of a 2-d data array
        SUBROUTINE ISTATS_CIRC(ARRAY,VAR,QUAL,XC,YC,R,
     :                                DMIN,DMAX,MEAN,TOTAL,MERR,
     :                                 NGOOD,NBAD,XCENT,YCENT,STATUS)
*    Description :
*       If quality information is available then each pixel is checked and
*       bad ones are excluded from the stats calculation.
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL ARRAY(I_NX,I_NY)                ! Data array
      REAL VAR(I_NX,I_NY)                  ! Variance array
      BYTE QUAL(I_NX,I_NY)                 ! Quality array
      REAL XC,YC,R
*    Import-Export :
*    Export :
      REAL DMIN
      REAL DMAX
      REAL MEAN                            ! Mean value of pixels found in
*                                          !   subset of array
      REAL TOTAL
      REAL MERR                            ! Error on the above mean
      INTEGER NGOOD                        ! Number of good pixels
      INTEGER NBAD                         ! Number of bad pixels
      REAL XCENT,YCENT
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
      LOGICAL IMG_INCIRC
*    Local variables :
      REAL VALVAR			! value weighted by variance
      REAL XPIX,YPIX
      INTEGER IX,IY
      INTEGER I1,I2,J1,J2
      LOGICAL GOOD
*-
      IF (STATUS.NE.SAI__OK) RETURN

* Initialise:
      TOTAL=0.0
      VALVAR=0.0
      NGOOD=0
      NBAD=0
      DMIN=I_DMAX
      DMAX=I_DMIN
      XPIX=0.0
      YPIX=0.0


* Loop over all pixels within rectangle enclosing circle
      CALL IMG_CIRCTOBOX(XC,YC,R,I1,I2,J1,J2,STATUS)
      DO IY=J1,J2
        DO IX=I1,I2

* check pixel is within circle
          IF (IMG_INCIRC(IX,IY,XC,YC,R)) THEN

*   Test pixel quality
            IF (I_BAD) THEN
              GOOD=(BIT_ANDUB(QUAL(IX,IY),I_MASK).EQ.QUAL__GOOD)
            ELSE
              GOOD=.TRUE.
            ENDIF

            IF (GOOD) THEN
*     Sum the pixel values
               TOTAL=TOTAL+ARRAY(IX,IY)
               DMIN=MIN(ARRAY(IX,IY),DMIN)
               DMAX=MAX(ARRAY(IX,IY),DMAX)
               XPIX=XPIX+REAL(IX)*ARRAY(IX,IY)
               YPIX=YPIX+REAL(IY)*ARRAY(IX,IY)

               NGOOD=NGOOD+1
*
            ELSE
*
               NBAD=NBAD+1
*
            ENDIF

          ENDIF

        ENDDO
*
      ENDDO
*
* Normalise
      IF (NGOOD .GT. 0) THEN
*
         MEAN=TOTAL/REAL(NGOOD)
         XPIX=XPIX/TOTAL
         YPIX=YPIX/TOTAL
         CALL IMG_PIXTOWORLD(XPIX,YPIX,XCENT,YCENT,STATUS)
         CALL ISTATS_CIRC_MEANVAR(XC,YC,R,I1,I2,J1,J2,
     :                        ARRAY,QUAL,MEAN,MERR,STATUS)
*
      ENDIF
*
*
      END
*

        SUBROUTINE ISTATS_CIRC_MEANVAR(XC,YC,R,I1,I2,J1,J2,
     :                            ARRAY,QUAL,MEAN,MERR,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL XC,YC,R
      INTEGER I1,I2,J1,J2
      REAL ARRAY(I_NX,I_NY)                ! Data array
      BYTE QUAL(I_NX,I_NY)                 ! Quality array
      REAL MEAN                            ! Mean value of pixels found in
*    Import-Export :
*    Export :
      REAL MERR                            ! Error on the above mean
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
      LOGICAL IMG_INCIRC
*    Local variables :
      REAL XPIX,YPIX
      REAL RGOOD
      INTEGER IX,IY
      INTEGER NGOOD
      LOGICAL GOOD
*-
      IF (STATUS.NE.SAI__OK) RETURN

* Initialise:
      NGOOD=0
      MERR=0.0


* Loop over all pixels within rectangle enclosing circle
      DO IY=J1,J2
        DO IX=I1,I2

* check pixel is within circle
          IF (IMG_INCIRC(IX,IY,XC,YC,R)) THEN

*   Test pixel quality
            IF (I_BAD) THEN
              GOOD=(BIT_ANDUB(QUAL(IX,IY),I_MASK).EQ.QUAL__GOOD)
            ELSE
              GOOD=.TRUE.
            ENDIF

            IF (GOOD) THEN
              MERR=MERR+(ARRAY(IX,IY)-MEAN)**2
*
              NGOOD=NGOOD+1
*
*
            ENDIF

          ENDIF

        ENDDO
*
      ENDDO
*
* Normalise
      IF (NGOOD .GT. 1) THEN
*
        MERR=SQRT(MERR)
        RGOOD=REAL(NGOOD)
        MERR=MERR/SQRT(RGOOD)/SQRT(RGOOD-1.0)

*
      ENDIF
*
*
      END
*





*+ ISTATS_REG      Integrates the pixels in a subset of a 2-d data array
        SUBROUTINE ISTATS_REG(ARRAY,VAR,QUAL,DMIN,DMAX,MEAN,TOTAL,MERR,
     :                                 NGOOD,NBAD,XCENT,YCENT,STATUS)
*    Description :
*       If quality information is available then each pixel is checked and
*       bad ones are excluded from the stats calculation.
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL ARRAY(I_NX,I_NY)                ! Data array
      REAL VAR(I_NX,I_NY)                  ! Variance array
      BYTE QUAL(I_NX,I_NY)                 ! Quality array
*    Import-Export :
*    Export :
      REAL DMIN
      REAL DMAX
      REAL MEAN                            ! Mean value of pixels found in
*                                          !   subset of array
      REAL TOTAL
      REAL MERR                            ! Error on the above mean
      INTEGER NGOOD                        ! Number of good pixels
      INTEGER NBAD                         ! Number of bad pixels
      REAL XCENT,YCENT
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
      LOGICAL IMG_INREG
*    Local variables :
      REAL VALVAR			! value weighted by variance
      REAL XPIX,YPIX
      INTEGER IX,IY
      LOGICAL GOOD
*-
      IF (STATUS.NE.SAI__OK) RETURN

* Initialise:
      TOTAL=0.0
      VALVAR=0.0
      NGOOD=0
      NBAD=0
      DMIN=I_DMAX
      DMAX=I_DMIN
      XPIX=0.0
      YPIX=0.0


* Loop over all pixels within rectangle enclosing region
      DO IY=I_IY1,I_IY2
        DO IX=I_IX1,I_IX2

* check pixel is within circle
          IF (IMG_INREG(IX,IY)) THEN

*   Test pixel quality
            IF (I_BAD) THEN
              GOOD=(BIT_ANDUB(QUAL(IX,IY),I_MASK).EQ.QUAL__GOOD)
            ELSE
              GOOD=.TRUE.
            ENDIF

            IF (GOOD) THEN
*     Sum the pixel values
               TOTAL=TOTAL+ARRAY(IX,IY)
               DMIN=MIN(ARRAY(IX,IY),DMIN)
               DMAX=MAX(ARRAY(IX,IY),DMAX)
               XPIX=XPIX+REAL(IX)*ARRAY(IX,IY)
               YPIX=YPIX+REAL(IY)*ARRAY(IX,IY)

               NGOOD=NGOOD+1
*
            ELSE
*
               NBAD=NBAD+1
*
            ENDIF

          ENDIF

        ENDDO
*
      ENDDO
*
* Normalise
      IF (NGOOD .GT. 0) THEN
*
         MEAN=TOTAL/REAL(NGOOD)
         XPIX=XPIX/TOTAL
         YPIX=YPIX/TOTAL
         CALL IMG_PIXTOWORLD(XPIX,YPIX,XCENT,YCENT,STATUS)
         CALL ISTATS_REG_MEANVAR(ARRAY,QUAL,MEAN,MERR,STATUS)
*
      ENDIF
*
*
      END
*

        SUBROUTINE ISTATS_REG_MEANVAR(ARRAY,QUAL,MEAN,MERR,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL ARRAY(I_NX,I_NY)                ! Data array
      BYTE QUAL(I_NX,I_NY)                 ! Quality array
      REAL MEAN                            ! Mean value of pixels found in
*    Import-Export :
*    Export :
      REAL MERR                            ! Error on the above mean
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
      LOGICAL IMG_INREG
*    Local variables :
      REAL XPIX,YPIX
      REAL RGOOD
      INTEGER IX,IY
      INTEGER NGOOD
      LOGICAL GOOD
*-
      IF (STATUS.NE.SAI__OK) RETURN

* Initialise:
      NGOOD=0
      MERR=0.0


* Loop over all pixels within rectangle enclosing circle
      DO IY=I_IY1,I_IY2
        DO IX=I_IX1,I_IX2

* check pixel is within circle
          IF (IMG_INREG(IX,IY)) THEN

*   Test pixel quality
            IF (I_BAD) THEN
              GOOD=(BIT_ANDUB(QUAL(IX,IY),I_MASK).EQ.QUAL__GOOD)
            ELSE
              GOOD=.TRUE.
            ENDIF

            IF (GOOD) THEN
              MERR=MERR+(ARRAY(IX,IY)-MEAN)**2
*
              NGOOD=NGOOD+1
*
*
            ENDIF

          ENDIF

        ENDDO
*
      ENDDO
*
* Normalise
      IF (NGOOD .GT. 1) THEN
*
        MERR=SQRT(MERR)
        RGOOD=REAL(NGOOD)
        MERR=MERR/SQRT(RGOOD)/SQRT(RGOOD-1.0)

*
      ENDIF
*
*
      END
*
