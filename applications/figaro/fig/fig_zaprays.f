C+
      SUBROUTINE FIG_ZAPRAYS (DATA,VEXIST,VARIANCE,NX,NY,LX1,LX2,LY1,
     :                        LY2,MAXBAD,NSIG,FRAC,EXCMIN,SHARPNESS,
     :                        CRSHARPNESS,TEXTFILE,IMAGENAME,FLAG,
     :                        BADAREA,NBADAREA,STATUS)
C
C     F I G _ Z A P R A Y S
C
C     Attempts to detect cosmic rays in an image - probably a CCD
C     image.  A point is regarded as a possible cosmic ray if it
C     is the largest of the cross of five points centered on it.  If
C     its excess over the average of the five points is a) greater
C     than a given number of sigma over the average, and b) greater
C     than a given fraction of the average, and c) greater than some
C     given absolute value, and d) satisfies the sharpness criterion
C     (if specified), it is regarded as a cosmic ray, and the
C     block of 9 pixels surrounding it is zapped.  It is also assumed
C     that a cosmic ray will be centered on a positive pixel.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (!) DATA      (Real array DATA(NX,NY)) The image data.
C     (>) VEXIST    (Logical) TRUE if the variance array exists
C     (!) VARIANCE  (Real array VARIANCE(NX,NY)) The image variances.
C     (>) NX        (Integer) The first dimension of DATA and VARIANCE
C     (>) NY        (Integer) The second dimension of DATA and VARIANCE.
C     (>) LX1       (Integer) A subset of DATA given defined by the
C     (>) LX2       (Integer) limits LY1,LY2 (in the 2nd dimension) and
C     (>) LY1       (Integer) LX1,LX2 (in the first dimension) is checked
C     (>) LY2       (Integer) for cosmic rays.
C     (>) MAXBAD    (Integer) The maximum number of cosmic rays allowed
C     (>) NSIG      (Real) The number of sigma to be exceeded
C     (>) FRAC      (Real) The fraction to be exceeded
C     (>) EXCMIN    (Real) The minimum value to be exceeded
C     (>) SHARPNESS (Logical) Whether the sharpness test will be performed
C     (>) CRSHARPNESS (Real) The sharpness parameter to be exceeded
C     (>) TEXTFILE  (Logical) Whether a text file of results will be written
C     (>) IMAGENAME (Character) The name of the input image being examined
C     (>) FLAG      (Real) The value to be used to flag invalid pixels.
C     (!) BADAREA   (Integer array BADAREA(4,MAXBAD)) The limits of the
C                   areas zapped.
C     (!) NBADAREA  (Integer) The number of bad areas found so far.
C     (<) STATUS    (Integer) Status return.  0=> OK, 1 => Too many
C                   cosmic rays found.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     FIG_CLOSEST   (FIG_ package) Get nearest valid pixel value
C     FIG_PUNCHOUT  (FIG_ package) Flag area of image as invalid
C     DSA_OPEN_TEXT_FILE (DSA_ package) Opens a text file
C
C     This routine is based on an original routine by John Tonry.
C
C                                              KS / CIT 27th Feb 1984
C     Modified:
C
C     26th Aug 1988 - MCBA / AAO. Added CRSHARPNESS, SHARPNESS, TEXTFILE,
C                     and IMAGENAME.
C     6th  Oct 1992 - HME / UoE, Starlink.  Lowercase file name
C                     bclean.lis.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,LX1,LX2,LY1,LY2,MAXBAD,BADAREA(4,MAXBAD)
      INTEGER NBADAREA,STATUS
      REAL    DATA(NX,NY),VARIANCE(NX,NY),NSIG,FRAC,EXCMIN,CRSHARPNESS
      REAL    FLAG
      LOGICAL SHARPNESS,TEXTFILE,VEXIST
      CHARACTER IMAGENAME*(*)
C
C     Functions used
C
      REAL FIG_CLOSEST
      EXTERNAL FIG_CLOSEST
      INTEGER ICH_LEN
C
C     Local variables
C
      LOGICAL CCURVE,PEAK
      INTEGER IX,IXL1,IXL2,IY,IYL1,IYL2
      REAL    AVE,AVEX,AVEY,CURVE,D0,D1,D2,D3,D4,D5,D6,D7,D8,DDX2,DDY2
      REAL    S1,S2,S3,S4,S5,S6,S7,S8,SHARP,SKY,WINGS,SHOULDER
      REAL    EXCESS,SIGMA,RSIGMA,RFRAC,MAX_TO_PRINT
      INTEGER   TEXTLU     ! the logical unit for the text file
      INTEGER   NPOSSIBLES ! the number of possible cosmic rays found
      CHARACTER TEXTNAME*1 ! scratch storage for DSA_OPEN_TEXT_FILE
      CHARACTER RAY*3      ! 'YES' or ' N ' depending on whether we have a ray
      INTEGER   IGNORE     ! Used when ignoring status returns
C
      STATUS=0
C
      IF (TEXTFILE) THEN
         CALL DSA_OPEN_TEXT_FILE ('bclean.lis','bclean.lis','NEW',
     &      .TRUE.,TEXTLU,TEXTNAME,STATUS)
         IF (STATUS.NE.0) GO TO 500
         CALL FIG_TEXT_FILE_HEADER (TEXTLU,'BCLEAN')
         WRITE (TEXTLU,100,ERR=201) IMAGENAME(:ICH_LEN(IMAGENAME)),
     &                      NSIG,FRAC,EXCMIN
100      FORMAT (
     & 'Image file: ',A//
     & 'This file contains a list of all pixels (X,Y) in the image ',
     &                                                 'which satisfy'/
     & 'the following criteria:'//
     & '(1) The value of the data at (X,Y) is positive,'/
     & '(2) (X,Y) exceeds the average of the four nearest pixels by',
     &                                                         ' more'/
     & '    than CRSIG times the square root of that average,'/
     & '(3) (X,Y) exceeds the average of the four nearest pixels by',
     &                                                         ' more'/
     & '    than CRFACT times that average, and'/
     & '(4) (X,Y) exceeds the average of the four nearest pixels by',
     &                                                         ' more'/
     & '    than CRMINV.'//
     & 'The values of the parameters were:'//
     & '    CRSIG       =',G15.7/
     & '    CRFACT      =',G15.7/
     & '    CRMINV      =',G15.7/)
C
C Check to see if the sharpness test was requested. Annotate the output
C file accordingly.
C
         IF (SHARPNESS) THEN
           WRITE (TEXTLU,101,ERR=201) CRSHARPNESS
101        FORMAT (
     & 'A sharpness test was also performed. This test measures the ',
     &                                               'height of the'/
     & '"core" of the potential cosmic ray above the sky background,',
     &                                               ' and compares'/
     & 'it with the height of the "wings" above the sky. If the rati',
     &                                               'o of these two'/
     & 'heights exceeds CRSHARPNESS, then the pixel (X,Y) is tagged ',
     &                                               'as a cosmic'/
     & 'ray. The "core" value is taken as (X,Y); the "wings" value i',
     &                                               's taken as the'/
     & 'second lowest of the 8 pixels immediately surrounding (X,Y);',
     &                                               ' the sky'/
     & 'background is estimated as the second lowest of the 8 pixels',
     &                                               ' (X-2,Y-2),'/
     & '(X,Y-2), (X+2,Y-2), (X-2,Y), (X+2,Y), (X-2,Y+2), (X,Y+2), (X',
     &                                               '+2,Y+2).'//
     & 'The value of the sharpness parameter was:'//
     & '    CRSHARPNESS =',G15.7/)
        ELSE
           WRITE (TEXTLU,105,ERR=201)
105        FORMAT ('A sharpness test was NOT performed.'//)
        END IF
        WRITE (TEXTLU,106,ERR=201)
106     FORMAT (
     &'In the following table, "Core" is the data value at pixel (X,Y',
     &                                                 '), "Wings" and'/
     &'"Sky" are calculated during the sharpness test (if requested),',
     &                                                 ' "Average" is'/
     &'the average of the four nearest pixels to (X,Y), "Excess" is  ',/
     &'"Core"-"Average", "Sigma" is "Excess"/SQRT("Average") , "Frac"',
     &                                                 ' is'/
     &'"Excess"/"Average", "Sharpness" is ("Core"-"Wings")/("Wings"-"',
     &                                                 'Sky"), and'/
     &'"Ray?" tells you whether (X,Y) has been considered to be a cos',
     &                                                 'mic ray.'//
     & ' Num    X    Y    Core   Wings     Sky Average  Excess Sigma',
     &                                            '  Frac Sharp Ray?'/
     & 77('-'))
      END IF
201   CONTINUE
C
C     Loop through all pixels in selected area
C
      CCURVE=.FALSE.
      NPOSSIBLES=0
      DO IY=MAX(2,LY1),MIN(NY-1,LY2)
         DO IX=MAX(2,LX1),MIN(NX-1,LX2)
C
C           Get the 5 cross pixels
C
            PEAK=.FALSE.
            RAY=' N '
            D0=DATA(IX,IY)
            IF (D0.GT.0.) THEN
               D1=FIG_CLOSEST(DATA,NX,NY,IX+1,IY,FLAG)
               IF (D0.GT.D1) THEN
                  D2=FIG_CLOSEST(DATA,NX,NY,IX,IY+1,FLAG)
                  IF (D0.GT.D2) THEN
                     D3=FIG_CLOSEST(DATA,NX,NY,IX-1,IY,FLAG)
                     IF (D0.GT.D3) THEN
                        D4=FIG_CLOSEST(DATA,NX,NY,IX,IY-1,FLAG)
                        PEAK=D0.GT.D4
                     END IF
                  END IF
               END IF
            END IF
C
C           Do we have a peak?
C
            IF (PEAK) THEN
C
C              Check excess (note - curve is the gaussian curvature,
C              ie a sharpness parameter.  This is a potential
C              discriminant, but is unused here. (The code is 'dormant'!)
C
C NOTE: this sharpness parameter does not appear to have any ability to
C       predict whether you have a cosmic ray or a star. MCBA. 14-Aug-88.
C
               AVEY=.5*(D2+D4)
               AVEX=.5*(D1+D3)
               AVE=(AVEY+AVEX)/2
               IF (CCURVE) THEN
                  DDY2=MAX(-1.0E18,MIN(D1+D3-2.*D0,1.0E18))
                  DDX2=MAX(-1.0E18,MIN(D2+D4-2.*D0,1.0E18))
                  CURVE=DDX2*DDY2
                  IF (CURVE.GT.0.) THEN
                     CURVE=ALOG10(CURVE)
                  ELSE IF (CURVE.LT.0.) THEN
                     CURVE=-ALOG10(-CURVE)
                  END IF
               END IF
               SIGMA=SQRT(ABS(AVE))
               EXCESS=D0-AVE
C
               IF (EXCESS.GT.MAX(NSIG*SIGMA,FRAC*AVE,EXCMIN)) THEN
                  NPOSSIBLES=NPOSSIBLES+1
C
                  IF (SHARPNESS) THEN
                     S1=FIG_CLOSEST(DATA,NX,NY,IX-2,IY-2,FLAG)
                     S2=FIG_CLOSEST(DATA,NX,NY,IX-2,IY  ,FLAG)
                     S3=FIG_CLOSEST(DATA,NX,NY,IX-2,IY+2,FLAG)
                     S4=FIG_CLOSEST(DATA,NX,NY,IX  ,IY-2,FLAG)
                     S5=FIG_CLOSEST(DATA,NX,NY,IX  ,IY+2,FLAG)
                     S6=FIG_CLOSEST(DATA,NX,NY,IX+2,IY-2,FLAG)
                     S7=FIG_CLOSEST(DATA,NX,NY,IX+2,IY  ,FLAG)
                     S8=FIG_CLOSEST(DATA,NX,NY,IX+2,IY+2,FLAG)
C
                     D5=FIG_CLOSEST(DATA,NX,NY,IX+1,IY-1,FLAG)
                     D6=FIG_CLOSEST(DATA,NX,NY,IX+1,IY+1,FLAG)
                     D7=FIG_CLOSEST(DATA,NX,NY,IX-1,IY-1,FLAG)
                     D8=FIG_CLOSEST(DATA,NX,NY,IX-1,IY+1,FLAG)
C
                     CALL FIG_2ND_LOWEST (SKY  ,S1,S2,S3,S4,S5,S6,S7,S8)
                     CALL FIG_2ND_LOWEST (WINGS,D1,D2,D3,D4,D5,D6,D7,D8)
C
                     SHOULDER=WINGS-SKY
                     IF (SHOULDER.LE.0) THEN
                        SHOULDER=1.0
                     END IF
C
                     SHARP=(D0-WINGS)/SHOULDER
C
                     IF (SHARP.GT.CRSHARPNESS) THEN
                        RAY='YES'
                     END IF
C
                  ELSE
                    RAY='YES'
                    SHARP=0.0
                    WINGS=0.0
                    SKY=0.0
                  END IF
C
C                 Do we want a text file produced?
C
                  IF (TEXTFILE) THEN
                     IF (SIGMA.NE.0) THEN
                       RSIGMA=EXCESS/SIGMA
                       RFRAC=EXCESS/AVE
                     ELSE
                       RSIGMA=0.0
                       RFRAC=0.0
                     END IF
C
C An example of the output format.
C
C Num    X    Y    Core   Wings     Sky Average  Excess Sigma  Frac Sharp Ray?
C-----------------------------------------------------------------------------
C   1 1234 1234-1.2E-33-1.2e-33-1.2e-33-1.2e-33-1.2e-33   5.4   1.7     2 YES
C   1  232  512      12      34      56     23       10   2.2   1.9    10  N
C   1   12 1023  2.1232  2.2323  1.2345  5.4321  4.5678  78.2  21.3   123 YES
C
C We try to take reasonable care that the output format looks nice.
C
                     MAX_TO_PRINT=MAX(D0,MAX(WINGS,MAX(SKY,
     &                                               MAX(AVE,EXCESS))))
                     IF ((EXCESS.GT.10).AND.
     &                   (MAX_TO_PRINT.LT.9999999)) THEN
                       WRITE (TEXTLU,102,ERR=200) NPOSSIBLES,IX,IY,
     &                    nint(D0),nint(WINGS),nint(SKY),nint(AVE),
     &                    nint(EXCESS),RSIGMA,RFRAC,nint(SHARP),RAY
102                    FORMAT (I4,2I5,5I8,2F6.1,I6,' ',A)
                     ELSE IF ((EXCESS.LE.10).AND.(EXCESS.GT.0.02).AND.
     &                        (MAX_TO_PRINT.LT.99.9)) THEN
                       WRITE (TEXTLU,103,ERR=200) NPOSSIBLES,IX,IY,
     &                    D0,WINGS,SKY,AVE,
     &                    EXCESS,RSIGMA,RFRAC,nint(SHARP),RAY
103                    FORMAT (I4,2I5,5F8.4,2F6.1,I6,' ',A)
                     ELSE
                       WRITE (TEXTLU,104,ERR=200) NPOSSIBLES,IX,IY,
     &                    D0,WINGS,SKY,AVE,
     &                    EXCESS,RSIGMA,RFRAC,nint(SHARP),RAY
104                    FORMAT (1P,I4,2I5,5G8.1,2F6.1,I6,' ',A)
                     END IF
200                  CONTINUE
                  END IF
C
C                    Have we got a ray?
C
                  IF (RAY.EQ.'YES') THEN
C
C                 Got a cosmic ray.  Zap it.
C
                     NBADAREA=NBADAREA+1
                     IF (NBADAREA.GT.MAXBAD) THEN
                        STATUS=1
                        GO TO 600
                     END IF
                     IXL1=MAX(1,IX-2)
                     IXL2=MIN(NX,IX+2)
                     IYL1=MAX(1,IY-2)
                     IYL2=MIN(NY,IY+2)
                     BADAREA(1,NBADAREA)=IXL1
                     BADAREA(2,NBADAREA)=IYL1
                     BADAREA(3,NBADAREA)=IXL2
                     BADAREA(4,NBADAREA)=IYL2
                     CALL FIG_PUNCHOUT(DATA,NX,NY,IXL1,IYL1,IXL2,IYL2,
     &                                                            FLAG)
C
C     If the variance array exists, set bad values to zero
C
                     IF (VEXIST) THEN
                       CALL FIG_PUNCHOUT(VARIANCE,NX,NY,IXL1,IYL1,IXL2,
     &                                   IYL2,0.0)
                     ENDIF
                  END IF
               END IF
            END IF
         END DO
      END DO
C
  600 CONTINUE
      IF (TEXTFILE) THEN
         IF (NBADAREA.EQ.0) THEN
           WRITE (TEXTLU,107,ERR=500)
107        FORMAT (///'No cosmic rays were found.')
         END IF
         CALL PAR_WRUSER ('Output written to BCLEAN.LIS.', IGNORE)
      END IF
  500 CONTINUE
      END
