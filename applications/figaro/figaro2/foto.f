C+
      SUBROUTINE FOTO
C
C     F O T O
C
C     Main routine for the Figaro aperture photometry function, FOTO.
C     This gets the required parameter values and then leaves the real
C     work to PHOTSUB, which is a modification for Figaro of a routine
C     originally written at KPNO by Don Wells.
C
C     Command parameters -
C
C     IMAGE    (Character) The name of the image being analysed.
C     RA1      (Numeric) The inner radius in pixels of the sky aperture.
C     RA2      (Numeric) The outer   "     "   "    "    "   "    "
C     NRADII   (Numeric) The number of apertures to use.
C     RADII    (Numeric array) The values of the radii, in pixels.
C     BAD      (Numeric) Bad pixel rejection threshold.
C     PHOTONS  (Numeric) Number of photons per ADU.
C     READNS   (Numeric) Readout noise.
C     MZERO    (Numeric) Magnitude offset.
C     SKYVAL   (Numeric) User-specified sky value.
C     SIGMA    (Numeric) User-specified sigma.
C
C     Command keywords -
C
C     DISHIST  Used to ask whether sky histogram to be displayed.
C     CONFIRM  Used to confirm acceptability of sky value and sigma.
C
C     User variables used -  (">" input, "<" output)
C
C     (>) SOFT  (Character) The soft plot device/type, as required by
C               PGPLOT.
C
C     Input -
C
C     CENTER.DAT contains one record for each point, giving
C                XCENT,YCENT,IX,IY,DX,DY,AP
C                in the format 2F8.2,2I5,2F8.2,I4 where
C                XCENT,YCENT give the position of the centroid
C                IX,IY are the original pixel position of the point.
C                DX,DY are the offsets in X and Y, and
C                AP is the value used for APERTURE.
C                If the centroid for a point cannot be determined, a
C                record is written giving
C                '*** No centroid ',IX,IY,DX,DY,AP
C                in the format A,2I5,2F8.2,I4.
C
C     Output -
C
C     MAGS.DAT   lists the magnitudes as determined by the program.
C
C                                           KS / CIT 1st June 1983
C     Modified:
C
C     6th  Aug 1987  Revised DSA_ routines - some specs changed. Now
C                    uses DYN_ routines for dynamic memory handling.
C     22nd Mar 1988  KS/AAO. Conversion to DSA completed.
C     4th  Sep 1992  Removed TABs, lowercase file names, changed
C                    INCLUDE, Changed MAX-declaration to *.
C                    Call GEN_TIME instead of VAX routine DATE. HME/UoE.
C     25th Sep 1992  HME / UoE, Starlink.  No more confusion about
C                    Fortran units: PKREAD is now called with IOUT,NFOR
C                    instead of 2,3; PKREAD gets a free unit number from
C                    DSA before opening NFOR (formerly 3).
C                    Finally had to use FORCHECK to find some instance
C                    of using logical operators on integers.
C     21st Jul 1993  HME / UoE, Starlink. More robust coding in MMM to
C                    avoid negative variance, and if it occurs to take
C                    the square root of its absolute value.
C     27th Jul 1993  HME / UoE, Starlink. Disuse PAR_Q, use PAR_ABORT.
C                    Add parameters DISHIST, CONFIRM, SKYVAL, SIGMA.
C     18th Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     6th  Aug 1997  MJCL / Starlink, UCL.  Explicit typecast of
C                    the fourth argument to PGPOINT due to Solaris
C                    problem.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_LEN
C
C     Maximum number of radii - note: more than 7 will cause problems
C     with the formatting of the output file (which is messy as it is).
C
      INTEGER MAXR
      PARAMETER (MAXR=7)
C
C     Floating point value limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Local variables
C
      REAL         BAD           !
      CHARACTER    CDAY*9
      CHARACTER    CHOUR*12
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      DPTR          ! Dynamic-memory pointer to data array
      INTEGER      DSLOT         ! Map slot number of input data array
      LOGICAL      FAULT         ! Flags any non-DSA errors
      INTEGER      I             !
      INTEGER      IGNORE        ! Used to pass ignorable status
      CHARACTER    IMAGE*132     ! The actual name of the image file
      INTEGER      IOUT          !
      INTEGER      ISTAT         ! Status variable for non-DSA use
      INTEGER      LDATE
      INTEGER      LDAY
      INTEGER      LHOUR
      INTEGER      MSKY          !
      REAL         MZERO         !
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NI            !
      INTEGER      NR            !
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      REAL         PEAK(MAXR+2)  !
      REAL         PHOTON        !
      REAL         QEAK(MAXR+2)  !
      REAL         RA1           !
      REAL         RA2           !
      REAL         READNS        !
      REAL         RLIM          !
      REAL         RR(MAXR)      !
      INTEGER      RPTR          ! Dynamic-memory pointer to workspace1
      CHARACTER    SOFT*64       ! PGPLOT spec for plotting device
      INTEGER      SPTR          ! Dynamic-memory pointer to workspace2
      INTEGER      STATUS        ! DSA_ routines running error code
      CHARACTER    STRING*80     !
      CHARACTER    TODAY*20      !
      REAL         VALUE         ! Temporary real number
      INTEGER      WSLOT         ! Map slot number for workspace
      INTEGER      WSLOT2        ! Map slot number for workspace
      REAL         WW(MAXR)      !
C
C     Initial values
C
      FAULT=.TRUE.
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get image name, and open file for it.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      CALL DSA_GET_ACTUAL_NAME('IMAGE',IMAGE,STATUS)
C
C     Get dimensions of image data.
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('This is not an image - not 2D data',IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      NX=DIMS(1)
      NY=DIMS(2)
C
C     Map image data
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
C
C     Get the rest of the parameters
C
      RLIM=MAX(NX,NY)
      CALL PAR_RDVAL('RA1',0.,RLIM,2.,'Pixels',RA1)
      IF (PAR_ABORT()) GO TO 500
      CALL PAR_RDVAL('RA2',RA1,RLIM,10.,'Pixels',RA2)
      IF (PAR_ABORT()) GO TO 500
      CALL PAR_RDVAL('NRADII',1.,FLOAT(MAXR),1.,' ',VALUE)
      IF (PAR_ABORT()) GO TO 500
      NR=NINT(VALUE)
      DO I=1,NR
         RR(I)=FLOAT(I)
      END DO
      CALL PAR_RDARY('RADII',0.,100.,'Increasing','Pixels',NR,MAXR,RR)
      IF (PAR_ABORT()) GO TO 500
      CALL PAR_RDVAL('BAD',FMIN,FMAX,0.,' ',BAD)
      IF (PAR_ABORT()) GO TO 500
      CALL PAR_RDVAL('PHOTONS',0.,FMAX,1.,'Photons',PHOTON)
      IF (PAR_ABORT()) GO TO 500
      CALL PAR_RDVAL('READNS',0.,FMAX,0.,' ',READNS)
      IF (PAR_ABORT()) GO TO 500
      CALL PAR_RDVAL('MZERO',FMIN,FMAX,0.,'Magnitudes',MZERO)
      IF (PAR_ABORT()) GO TO 500
C
C     Get the plotting device for soft plots
C
      CALL VAR_GETCHR('SOFT',0,0,SOFT,ISTAT)
      IF (ISTAT.NE.0) SOFT=' '
C
C     Calculate a few values needed by PHOTSUB
C
      NI=2*INT(RA2+.5)+1
      MSKY=3.14159265*((RA2+.5)**2-(RA1+.5)**2)
C
C     Get workspace
C
      CALL DSA_GET_WORK_ARRAY(MSKY,'FLOAT',SPTR,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NI,'FLOAT',RPTR,WSLOT2,STATUS)
C
C     Open output file and write headings - messy use of run-time
C     formatting (STRING) is because of uncertainty about value of NR.
C
      CALL DSA_GET_LU(IOUT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      OPEN (UNIT=IOUT,FILE='mags.dat',STATUS='NEW',IOSTAT=ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL PAR_WRUSER('Unable to open MAGS.DAT file',IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      CALL GEN_TIME (0,CDAY,LDAY,TODAY,LDATE,CHOUR,LHOUR)
      WRITE (IOUT,300,IOSTAT=ISTAT) TODAY(:LDATE),IMAGE(:ICH_LEN(IMAGE))
  300 FORMAT(/10X,'F O T O  -  Aperture Photometry Results',10X,A,//,
     :        10X,'Image: ',A,//,4X,'Object',13X,'Apertures')
      STRING='(4X,''#'',5X,''X'',7X,''Y'',2X,'//
     :       CHAR(NR+ICHAR('0'))//'(F8.2,1X),'//
     :       '4X,''Sky'',4X,''Sigma'',5X,''Skew'')'
      WRITE (IOUT,STRING,IOSTAT=ISTAT) (RR(I),I=1,NR)
C
C     Now let PHOTSUB do the work
C
      CALL PHOTSUB(%VAL(CNF_PVAL(DPTR)),NX,NY,RA1,RA2,RR,NR,NI,MSKY,
     :             BAD,PHOTON,READNS,MZERO,SOFT,IOUT,
     :             %VAL(CNF_PVAL(SPTR)),%VAL(CNF_PVAL(RPTR)),WW,PEAK,
     :             QEAK)
      IF (PAR_ABORT()) GO TO 500
C
C     Tell user output file name
C
      INQUIRE (UNIT=IOUT,NAME=STRING)
      CALL PAR_WRUSER(' ',IGNORE)
      CALL PAR_WRUSER('Results written to '//STRING(:ICH_LEN(STRING)),
     :                IGNORE)
C
C     Finally, tidy up.
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE PHOTSUB(IMAGE,NX,NRW,RA1,RA2,RR,NR,NI,MSKY,BAD,
     :            PHOTON,READNS,MZERO,PLOTDE,IOUT,SKY,RAY,WW,PEAK,QEAK)
C
C     P H O T S U B
C
C     This subroutine calculates the concentric aperture photometry
C     for a set of points whose centers in the image are held in the
C     file 'CENTER.DAT'.
C
C     Parameters -  (">" input, "<" output, "W" work)
C
C     (>) IMAGE  (Real array IMAGE(NX,NRW)) The image data.
C     (>) NX     (Integer) The number of pixels in each row of IMAGE
C     (>) NRW    (Integer) The number of rows in IMAGE
C     (>) RA1    (Real) The inner radius of the sky aperture
C     (>) RA2    (Real) The outer radius of the sky aperture
C     (>) RR     (Real array RR(NR)) The radii of the concentric
C                apertures for the photometry.
C     (>) NR     (Integer) The number of apertures.
C     (>) NI     (Integer) The number of lines covered by the outer
C                sky radius - the program operates in a NI by NI
C                square around each point.
C     (>) MSKY   (Integer) The number of sky pixels.
C     (>) BAD    (Real) The bad pixel rejection threshold.
C     (>) PHOTON (Real) Number of photons per adu.
C     (>) READNS (Real) Readout noise.
C     (>) MZERO  (Real) Magnitude offset - added to all computed
C                magnitudes.
C     (>) PLOTDE (Character) Plotting device/type as required by PGPLOT
C     (>) IOUT   (Integer) Fortran unit number of output file - already
C                open.
C     (W) SKY    (Real array SKY(MSKY)) Used to accumulate the sky
C                pixel values
C     (W) RAY    (Real array RAY(NI)) Used to hold a line of the
C                square - see NI.
C     (W) WW     (Real array WW(NR)) Used to hold sums of weights for
C                the aperture radii.
C     (W) PEAK   (Real array PEAK(NR+5)) Used to hold a mixture of
C                information for each point.  PEAK(1) is the X position,
C                PEAK(2) is the Y position, PEAK(3..NR+2) are the sums
C                in the various annuli, PEAK(NR+3) is the mode of the sky
C                distribution, PEAK(NR+4) is the sigma and PEAK(NR+5) is
C                the skew.
C     (W) QEAK   (Real array QEAK(NR+2)) Used to hold the errors for
C                the corresponding annuli in PEAK.  QEAK(1 & 2) are unused.
C
C
C     History -  This routine was originally written at KPNO by Don
C                Wells.  It was modified slightly at Caltech by JRM,
C                who made it run using the early Figaro data
C                structure routines, and also added the error section
C                - the code connected with the array QUEAK.  Further
C                modified at CIT by KS to make it run as a Figaro
C                application: somewhat restructured, many of the
C                subroutine calls having been changed, making
C                use of the ability of Figaro to pass the whole image
C                array to the routine; the coordinate transform code
C                is explicitly bypassed, although not removed; all
C                parameters are now passed to the subroutine.  None of
C                these changes are particularly dramatic, and the
C                temptation to make stylistic changes was resisted,
C                so the result has a slight mixture of commenting
C                styles.  The real work is done by the routines CAP1
C                and MMM, and these have been left more or less as
C                they were.
C
C                             This version : KS / CIT  26th July 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NRW,NR,NI,MSKY,IOUT
      REAL IMAGE(NX,NRW),RA1,RA2,RR(NR),BAD,PHOTON,READNS,MZERO
      REAL SKY(MSKY),RAY(NI),WW(NR),PEAK(NR+5),QEAK(NR+2)
      CHARACTER*(*) PLOTDE
C
C     Functions
C
      LOGICAL LEGVAR,PAR_ABORT
      REAL RRR,TH,XP,YP
C
C     Floating point value limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Local variables
C
      LOGICAL PLHIST,PQUEST
      INTEGER I,I1,IOBJ,J,J1,J2,LASTF,NITER,NSKY,NZ3,NFOR
      REAL APAREA,CP,DTH,ERROR1,ERROR2,ERROR3,ERROR4,SF,VALUE
      REAL X,X1,X1P,X2,X2P,XC,Y,Y1,Y1P,Y2,Y2P,YC,ZMEAN,ZMEDIAN,ZREJECT
C
C     Flag value for blank pixels
C
      REAL INDEF
      PARAMETER (INDEF=-99999.)
C
C-----ARITHMETIC STATEMENT FUNCTIONS FOR THE COORDINATE TRANSFORMATIONS:
      XP(X,Y)=((X-X1)*COS(DTH)-(Y-Y1)*SIN(DTH))*SF+X1P
      YP(X,Y)=((X-X1)*SIN(DTH)+(Y-Y1)*COS(DTH))*SF+Y1P
C
      RRR(X2,X1,Y2,Y1)=SQRT( (X2-X1)**2+(Y2-Y1)**2)
      TH(Y2,Y1,X2,X1)=ATAN2(Y2-Y1,X2-X1)
C
C-----THESE VALUES EFFECTIVELY DUMMY OUT THE COORDINATE TRANSFORMS: {KS}
      X1=0
      Y1=0
      X1P=0
      Y1P=0
      Y2=0
      Y2P=0
      X2=1.
      X2P=1.
C
C-----DETERMINE COORDINATE TRANSFORMATION:
      SF=RRR(X2P,X1P,Y2P,Y1P)/RRR(X2,X1,Y2,Y1)
      DTH=TH(Y2P,Y1P,X2P,X1P)-TH(Y2,Y1,X2,X1)
C
C      PRINT  7,X1P-X1,Y1P-Y1,SF,DTH/.017453293
C    7 FORMAT(/' SHIFT=('F6.1','F6.1'), SCALE FACTOR='F6.4
C     $', DELTATHETA='F8.4' DEG'/)
C
C-----SET PROMPT FLAG FOR SKY DISTRIBUTION PLOTS
      PLHIST=.TRUE.
C
C-----SET LASTF TO SIGNAL 'NEW FILE' AND INITIALISE OBJECT NUMBER
      LASTF=1
      IOBJ=0
C
C-----GET COORDINATES OF NEXT OBJECT TO BE MEASURED:
   10 CALL PKREAD(NFOR,IOUT,PEAK,IOBJ,LASTF)
      IF(LASTF.NE.0)GO TO 90
C
C-----APPLY COORDINATE TRANSFORMATION:
      XC=XP(PEAK(1),PEAK(2))
      YC=YP(PEAK(1),PEAK(2))
C
C-----COMPUTE LIMITS OF THE SUBMATRIX:
      I1=XC-NI/2
      J1=YC-NI/2
      J2=YC+NI/2
C
C-----INITIALIZE BEFORE READING THE SUBMATRIX:
      CALL ZRAY(PEAK(3),NR+3)
      CALL ZRAY(WW,NR)
      NSKY=0
C
C-----READ THROUGH THE SUBMATRIX, EXTRACTING THE DATA WE NEED:
      DO 50 J=J1,J2
         CALL PRD2(IMAGE,NX,NRW,I1,J,NI,RAY)
         CALL CAP1(RAY,NI,(I1-1.0),FLOAT(J),XC,YC,RA1,RA2,RR,PEAK(3),
     $                                       WW,NR,SKY,MSKY,NSKY,BAD)
   50 CONTINUE
C
C-----CALL -MMM- TO GET THE MODE OF THE SKY DISTRIBUTION:
      CALL MMM(SKY,NSKY,NZ3,ZMEAN,ZMEDIAN,PEAK(NR+3),PEAK(NR+4),
     $         PEAK(NR+5),ZREJECT,NITER,1.0)
C
C-----PHTPLT PLOTS THE SKY DISTRIBUTION. {KS}
C
      IF (PLHIST) THEN
         CALL PAR_CNPAR('DISHIST')
         CALL PAR_RDKEY('DISHIST',.TRUE.,PQUEST)
         IF (PAR_ABORT()) RETURN
         IF (PQUEST) THEN
            CALL PHTPLT(PLOTDE,SKY,NSKY,ZMEAN,PEAK(NR+3),PEAK(NR+4))
            CALL PAR_CNPAR('CONFIRM')
            CALL PAR_RDKEY('CONFIRM',.TRUE.,PQUEST)
            IF (PAR_ABORT()) RETURN
            IF (.NOT.PQUEST) THEN
               VALUE=PEAK(NR+3)
               CALL PAR_CNPAR('SKYVAL')
               CALL PAR_RDVAL('SKYVAL',FMIN,FMAX,VALUE,' ',PEAK(NR+3))
               IF (PAR_ABORT()) RETURN
               VALUE=PEAK(NR+4)
               CALL PAR_CNPAR('SIGMA')
               CALL PAR_RDVAL('SIGMA',FMIN,FMAX,VALUE,' ',PEAK(NR+4))
               IF (PAR_ABORT()) RETURN
            END IF
         ELSE
            PLHIST=.FALSE.
         END IF
      END IF
C
C-----COMPUTE CUMULATIVE SUMS OF THE ANNULI, MINUS THE SKY:
      CP=0.
      DO 52 I=1,NR
        IF(.NOT.LEGVAR(PEAK(I+2))) CP=INDEF
        IF(LEGVAR(CP))CP=CP+(PEAK(I+2)-PEAK(NR+3)*WW(I))
        PEAK(I+2)=CP
   52 CONTINUE
C
C-----CONVERT THE SUMS TO MAGNITUDES:
      DO 56 I=1,NR
         IF(.NOT.LEGVAR(PEAK(I+2)))GO TO 54
         IF(PEAK(I+2).LT.0.0)GO TO 54
C----    THIS ERROR DET SEC IS AN ADDITION TO WELLS PROG
C----    ERRORS ARE 1.READOUT NOISE, 2.PHOTON STATISTICS- IMAGE,
C----          3.SKY UNCERTAINTY,4.PHOTON STATS- SKY  [UNITS ARE ADU]
         APAREA=3.14159*RR(I)**2
         ERROR1=SQRT(APAREA)*READNS/PHOTON
         ERROR2=SQRT(ABS(PEAK(I+2)/PHOTON))
         ERROR3=APAREA*PEAK(NR+4)/SQRT(ABS(FLOAT(MSKY)))
         ERROR4=SQRT(ABS(APAREA*PEAK(NR+3)/PHOTON))
         QEAK(I+2)=-2.5*ALOG10(PEAK(I+2)+SQRT(ERROR1**2+ERROR2**2
     $                               +ERROR3**2+ERROR4**2))+MZERO
         PEAK(I+2)=-2.5*ALOG10(PEAK(I+2))+MZERO
         QEAK(I+2)=PEAK(I+2)-QEAK(I+2)
         GO TO 56
   54    PEAK(I+2)=INDEF
         QEAK(I+2)=INDEF
   56 CONTINUE
C
C-----AND FINALLY WRITE OUT THE ANSWERS:
C     (NOTE THAT WE WRITE OUT THE ORIGINAL COORDINATES, NOT THE
C      TRANSFORMED ONES THAT WERE IN XC AND YC.)
      CALL PKWRT(IOUT,IOBJ,PEAK,QEAK,NR)
C
      GO TO 10
C
   90 CONTINUE
      END
C+
      SUBROUTINE PRD2(IMAGE,NX,NRW,IX,IR,NI,RAY)
C
C     P R D 2
C
C     Fills an array with data from a section of a row of the
C     image, setting any non-existent pixels to a flag value.
C     (The flag value used is -99999.)
C
C     This routine has been totally re-written for the new
C     Figaro version of PHOTSUB, being a combination of the two
C     routines PRD2 and BRAY.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IMAGE   (Real array IMAGE(NX,NRW)) The image data
C     (>) NX      (Integer) The number of pixels in a row of IMAGE
C     (>) NRW     (Integer) The number of rows in IMAGE
C     (>) IX      (Integer) The first element of the row required.
C     (>) IR      (Integer) The row required
C     (>) NI      (Integer) The number of elements to transfer
C     (<) RAY     (Real array RAY(NI)) The resulting row section.
C
C     Functions / subroutines used - Standard Fortran
C
C     Common variables used - None
C                                             KS / CIT 29th May 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NRW,IX,IR,NI
      REAL    IMAGE(NX,NRW),RAY(NI)
C
C     Conventional value used for pixels outside the image
C
      REAL FLAG
      PARAMETER (FLAG=-99999.)
C
C     Local variables
C
      INTEGER I,IPT,IXPT
C
C     Check for a legitimiate row number.
C
      IF ((IR.LT.1).OR.(IR.GT.NRW)) THEN
         DO I=1,NI
            RAY(I)=FLAG
         END DO
      ELSE
C
C        Row is OK, fill in 3 stages: any pixels before start of
C        image, pixels in image, pixels past end of image.
C
         IPT=1
         IF (IX.LT.1) THEN
            DO I=1,1-IX
               RAY(I)=FLAG
            END DO
            IPT=2-IX
            IXPT=1
         ELSE
            IXPT=IX
         END IF
C
         DO I=IXPT,MIN(IX+NI-1,NX)
            RAY(IPT)=IMAGE(I,IR)
            IPT=IPT+1
         END DO
C
         IF (IPT.LT.NI) THEN
            DO I=IPT,NI
               RAY(I)=FLAG
            END DO
         END IF
      END IF
C
      END
C+
      SUBROUTINE PKREAD(NFOR,IOUT,PEAK,IOBJ,LASTF)
C
C     P K R E A D
C
C     Reads a peak position from the file of image centers
C
C     This routine has been modified for the Figaro version of
C     PHOTSUB; the use of LASTF has changed to make the routine
C     re-usable - it no longer depends on a flag variable
C     initialised by a DATA statement to control file opening.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (!) NFOR    (Integer) Fortran unit number to be used for the
C                 file.
C     (>) IOUT    (Integer) Fortran unit number used for the output
C                 file.
C     (<) PEAK    (Real array PEAK(2)) The first element of PEAK
C                 is set to the X-position of the center read from
C                 the file, the second element to the Y-position.
C     (!) IOBJ    (Integer) Used to hold the current object number -
C                 will be set to 0 for the first call to PKREAD and
C                 from then on will be incremented by PKREAD.
C     (!) LASTF   (Integer) Used for file control.  If passed as 0
C                 PKREAD will assume the file is already open; otherwise
C                 it will open it.  PKREAD will always return LASTF
C                 as zero, up to the point the end-of-file is read,
C                 (or if some other I/O error occurs), when it returns
C                 it as -1.
C
C     Files used -  CENTER.DAT, see comments for PHOTSUB.
C
C     Subroutines / functions used - None
C
C     Common variables used - None
C
C                                               KS / CIT 14th June 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NFOR,IOUT,IOBJ,LASTF
      REAL PEAK(2)
C
C     Local variables
C
      LOGICAL REPEAT
      INTEGER STATUS
      CHARACTER*80 RECORD
C
      STATUS=0
C
      IF (LASTF.NE.0) THEN
         CALL DSA_GET_LU(NFOR,STATUS)
         OPEN(UNIT=NFOR,FILE='center.dat',STATUS='OLD',IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER(
     :            'Unable to open centroid file ("center.dat")',STATUS)
            GO TO 550
         END IF
      END IF
      REPEAT=.TRUE.
      DO WHILE (REPEAT)
C
C        Read record
C
         READ(NFOR,'(A)',END=550,ERR=550) RECORD
         IOBJ=IOBJ+1
C
C        Check for error flag - reread if so.
C
         IF (INDEX(RECORD,'**').NE.0) THEN
            WRITE(IOUT,'(I5,34X,A,/)') IOBJ,
     :                                 'Unable to determine centroid'
         ELSE
C
C           Decode position from record
C
            READ(RECORD,'(2F8.2)',ERR=550) PEAK(1),PEAK(2)
            REPEAT=.FALSE.
         END IF
      END DO
C
C     Normal exit
C
      LASTF=0
      GO TO 600
C
C     Error exit
C
  550 CONTINUE
      LASTF=-1
C
  600 CONTINUE
C
      END
C+
      LOGICAL FUNCTION LEGVAR(VALUE)
C
C     L E G V A R
C
C     Returns whether or not a value is a legitimate value or
C     is the 'blank pixel' flag value. (The value used is -99999.)
C
C     Parameters -  (">" input, "<" output)
C
C     (>) VALUE    (Real) The pixel value to be tested
C
C     Returns -
C
C     (<) LEGVAR   (Logical) True for a legitimate value, false
C                  for the flag value.
C
C                                            KS / CIT 30th May 1983
C+
      IMPLICIT NONE
C
C     Parameter
C
      REAL VALUE
C
      LEGVAR=VALUE.NE.-99999.
C
      END
C+
      SUBROUTINE PKWRT(IOUT,IOBJ,PEAK,QEAK,NR)
C
C     P K W R T
C
C     Writes the results for a point to the output file.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IOUT    (Integer) Fortran unit number for output file
C                 - should be already open.
C     (>) IOBJ    (Integer) The number of the object.
C     (>) PEAK    (Real array PEAK(NR+5)) Peak information - see
C                 PHOTSUB for full details.
C     (>) QEAK    (Real array QEAK(NR+2)) Error information - see
C                 PHOTSUB for full details.
C     (>) NR      (Integer) Number of apertures used.
C
C                                               KS / CIT 3rd June 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IOUT,IOBJ,NR
      REAL PEAK(NR+5),QEAK(NR+2)
C
C     Flag value for bad results
C
      REAL FLAG
      PARAMETER (FLAG=-99999.)
C
C     Local variables
C
      INTEGER I,IODUM,IPT
      CHARACTER LINE*132
C
      WRITE(LINE,100,IOSTAT=IODUM) IOBJ,(PEAK(I),I=1,NR+5)
  100 FORMAT(I5,2F8.2,10F9.3)
      DO I=3,NR+5
         IF (PEAK(I).EQ.FLAG) THEN
            IPT=(I-3)*9+22
            LINE(IPT:IPT+8)=' Bad Data'
         END IF
      END DO
      WRITE(IOUT,'(A)',IOSTAT=IODUM) LINE
      WRITE(LINE,101,IOSTAT=IODUM) (QEAK(I),I=3,NR+2)
  101 FORMAT(21X,10F9.3)
      DO I=3,NR+2
         IF (QEAK(I).EQ.FLAG) THEN
            IPT=(I-3)*9+22
            LINE(IPT:IPT+8)=' '
         END IF
      END DO
      WRITE(IOUT,'(A)',IOSTAT=IODUM) LINE
      END
C+
      SUBROUTINE PHTPLT(PLOTDE,SKY,NSKY,ZMODE,ZMEAN,ZSIGMA)
C
C     P H T P L T
C
C     Plots the sky distribution.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) PLOTDE    (Character) The plotting device/type in the
C                   form required by PGPLOT.
C     (>) SKY       (Real array SKY(NSKY)) The sky data.
C     (>) NSKY      (Integer) The number of sky points.
C     (>) ZMODE     (Real) The mode of the sky distribution.
C     (>) ZMEAN     (Real) The mean sky value.
C     (>) ZSIGMA    (Real) The sigma of the sky distribution.
C
C                                       KS / CIT 1st June 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NSKY
      REAL SKY(NSKY),ZMEAN,ZMODE,ZSIGMA
      CHARACTER*(*) PLOTDE
C
      CALL PGBEGIN(0,PLOTDE,1,1)
      CALL PGHIST(NSKY,SKY,ZMEAN-3.*ZSIGMA,ZMEAN+3.*ZSIGMA,50,0)
      CALL PGPOINT(1,ZMODE,0.,ICHAR('*'))
      CALL PGPOINT(1,ZMODE,10.,ICHAR('+'))
      CALL PGEND
C
      END
      SUBROUTINE MMM(ZRAY,NZ1,NZ3,ZMEAN,ZMEDIAN,ZMODE,ZSIGMA,ZSKEW,
     $        ZREJECT,NITER,RKLUGE)
C-----'FLOATING' REJECTION INSTALLED, SEPT 78, DCW.
C-----PLOTTING CODE MOVED TO SUBROUTINE PHTPLT, MAY 83, KS
C-----SORT NOW DONE USING GEN_QFSORT, RATHER THAN QUIK (IS FASTER, AND
C-----DOESN'T NEED DUMMY KEYS AT ENDS OF ARRAY).  MAY 83, KS
      REAL ZRAY(*)
      DOUBLE PRECISION S,SS
      LOGICAL LEGVAR
      DATA XINDEF/-99999./
C
      IF(NZ1.LE.0)GO TO 33
C
C-----EXTRACT NONBLANK PIXELS,SET CUT1=ZMIN,CUT2=ZMAX:
      N=0
      S=0.D0
      SS=0.D0
      DO 30 I=1,NZ1
      Z=ZRAY(I)
      IF(.NOT.LEGVAR(Z))GO TO 30
      IF(N.NE.0)GO TO 20
      CUT1=Z
      CUT2=Z
   20 CUT1=AMIN1(Z,CUT1)
      CUT2=AMAX1(Z,CUT2)
      N=N+1
      ZRAY(N)=Z
      S=S+DBLE(Z)
      SS=SS+DBLE(Z)*DBLE(Z)
   30 CONTINUE
C
C-----CHECK FOR AT LEAST ONE VALUE:
      IF(N.NE.0)GO TO 35
   33 NZ3=0
      NITER=0
      ZMEAN=XINDEF
      ZMEDIAN=XINDEF
      ZMODE=XINDEF
      ZSIGMA=XINDEF
      ZSKEW=XINDEF
      ZREJECT=XINDEF
      RETURN
C
C-----SORT THE DATA:
   35 CONTINUE
      CALL GEN_QFSORT(ZRAY,N)
C
C-----INITIALIZE FOR ITERATION LOOP:
      NIT=0
      J1=0
      NZ2=N
      J2=N
C
C-----THE BIG LOOP:
   40 NIT=NIT+1
      J1L=J1
      NZL=NZ2
C
C-----GET MEAN AND SIGMA:
      FNZL=NZL
      ZBAR=S/FNZL
      ZSIG=SQRT(ABS(SS/DBLE(FNZL)-DBLE(S/FNZL)*DBLE(S/FNZL)))
C
C-----GET MEDIAN:
C     (NOTE THAT DATA ARRAY STAYS SORTED EVEN WHEN VALUES ARE REJECTED)
      J=J1L+NZL/2
      ZMED=ZRAY(J+1)
C
C-----Who is responsible for this nonsense?
C
C     IF((NZL.AND.1).EQ.0)ZMED=0.5*(ZRAY(J)+ZMED)
      IF(MOD(NZL,2).EQ.0)ZMED=0.5*(ZRAY(J)+ZMED)
C
C-----GET MODE ESTIMATE:
      ZMOD=3.*ZMED-2.*ZBAR
C     PRINT *,CUT1,CUT2,ZBAR,ZMED,ZMOD,ZSIG,N
C
      IF(NIT.GT.10)GO TO 190
C
C-----GET CHAUVENET REJECTION CRITERION:
      R=RKLUGE
      IF(R.GT.1.0)GO TO 60
      R=ALOG10(FNZL*R)
      R=(-.1042*R+1.1695)*R+.8895
   60 R=AMAX1(R,2.0)
C
C-----GET REJECTION CUTS BASED ON MODE:
      CUT=R*ZSIG+0.5*ABS(ZBAR-ZMOD)
      CUT1=ZMOD-CUT
      CUT2=ZMOD+CUT
C
C-----NOW USE CUT1+CUT2 TO REJECT VALUES FROM LIST:
C     J1 POINTS TO LAST REJECTED VALUE AT BEGINNING OF LIST.
C     J2 POINTS TO LAST ACCEPTED VALUE IN LIST.
C     SO, J2-J1 IS THE NUMBER OF ACCEPTED VALUES IN THE LIST.
C     LIST IS SORTED, SO WE WILL SIMPLY STEP J1 AND J2 FORWARD OR
C     BACKWARD TO FIND WHERE CUT1 AND CUT2 FIT INTO THE LIST
C     OF SORTED VALUES. WE ADD OR SUBTRACT VALUES FROM THE S AND SS SUMS
C     AS WE MOVE THE J1 AND J2 POINTERS.
C     WE DEPEND ON THE 48-BIT MANTISSA OF THE 6400 TO MAINTAIN ENOUGH
C     PRECISION IN THE S AND SS SUMS SO THAT WE CAN CALCULATE -ZSIG-.
C
C     SHOULD CUT1 BE MOVED TOWARD MIDDLE OF LIST:
      IF(J1.EQ.N)GO TO 80
      Z=ZRAY(J1+1)
      IF(CUT1.LE.Z)GO TO 80
   70 S=S-Z
      SS=SS-Z*Z
      J1=J1+1
      IF(J1.EQ.N)GO TO 90
      Z=ZRAY(J1+1)
      IF(CUT1.GT.Z)GO TO 70
      GO TO 90
C     ELSE, SHOULD CUT1 BE MOVED TOWARD BEGINNING OF LIST:
   80 IF(J1.EQ.0)GO TO 90
      Z=ZRAY(J1)
      IF(CUT1.GT.Z)GO TO 90
      S=S+Z
      SS=SS+Z*Z
      J1=J1-1
      GO TO 80
C     SHOULD CUT2 BE MOVED TOWARD MIDDLE OF LIST:
   90 IF(J2.EQ.0)GO TO 110
      Z=ZRAY(J2)
      IF(CUT2.GE.Z)GO TO 110
  100 S=S-Z
      SS=SS-Z*Z
      J2=J2-1
      IF(J2.EQ.0)GO TO 120
      Z=ZRAY(J2)
      IF(CUT2.LT.Z)GO TO 100
      GO TO 120
C     ELSE, SHOULD CUT2 BE MOVED TOWARD END OF LIST:
  110 IF(J2.EQ.N)GO TO 120
      Z=ZRAY(J2+1)
      IF(CUT2.LT.Z)GO TO 120
      S=S+Z
      SS=SS+Z*Z
      J2=J2+1
      GO TO 110
C
  120 NZ2=J2-J1
C
C-----TEST TO EXIT LOOP:
      IF(NZ2.EQ.0)GO TO 190
      IF(J1.NE.J1L)GO TO 40
      IF(NZ2.NE.NZL)GO TO 40
C
C
C-----THATS ALL, FOLKS:
  190 NZ3=NZL
      ZMEAN=ZBAR
      ZMEDIAN=ZMED
      ZMODE=ZMOD
      ZSIGMA=ZSIG
      ZSKEW=(ZBAR-ZMOD)/ZSIG
      ZREJECT=R
      NITER=NIT
C
      END
      SUBROUTINE CAP1(ZZ,NZ,X0,Y0,XC,YC,RA1,RA2,RR,SS,WW,
     $                                        NR,AA,MA,NA,BAD)
      REAL ZZ(NZ),RR(NR),SS(NR),WW(NR),AA(MA)
      LOGICAL LEGVAR
C
C-----ZZ(NZ) =INPUT ARRAY
C-----X0,Y0 =COORS OF ZZ(0)
C-----XC,YC =COORS OF 'CENTER'
C-----RA1,RA2 =INNER+OUTER RADII OF SKY ANNULUS
C-----RR(NR) =RADII OF CONCENTRIC APERTURES IN ASCENDING ORDER.
C-----SS(NR) =SUMS OF FLUX FOR RADII (INITIALLY ZERO)
C-----WW(NR) =SUMS OF WEIGHT FOR RADII (INITIALLY ZERO)
C-----AA(MA) =ARRAY TO ACCUMULATE SKY PIXELS
C-----NA=NUMBER OF PIXELS CURRENTLY IN AA()
C-----BAD =BAD PIXEL REJECTION THRESHOLD
C
      X=X0-XC
      DY2=(Y0-YC)**2
C
C-----LOOP OVER PIXELS:
      DO 30 I=1,NZ
      X=X+1.0
      R=SQRT(X*X+DY2)
C
C-----DOES THIS PIXEL GO INTO SKY ANNULUS:
      IF(R.LT.RA1)GO TO 10
      IF(R.GT.RA2)GO TO 10
      IF(NA.GE.MA)GO TO 10
      IF((.NOT.LEGVAR(ZZ(I))).OR.(ZZ(I).LT.BAD))GO TO 10
      NA=NA+1
      AA(NA)=ZZ(I)
C
C-----CHECK ANNULI:
   10 IF(R.GT.(RR(NR)+0.5))GO TO 30
      DR2=1.0
C
C-----THIS IS A SNEAKY ALGORITHM:
      DO 20 J=1,NR
      DR1=1.0-DR2
      DR2=AMAX1(0.0,AMIN1(R+0.5-RR(J),1.0))
      D=1.0-DR1-DR2
      IF(D.EQ.0.0)GO TO 20
      IF((.NOT.LEGVAR(ZZ(I))).OR.(ZZ(I).LT.BAD))GO TO 15
      IF(.NOT.LEGVAR(SS(J))) GO TO 15
      IF(BAD.EQ.10.1)WRITE(99,*)ZZ(I),D
      SS(J)=SS(J)+ZZ(I)*D
      WW(J)=WW(J)+D
      GO TO 17
   15 SS(J)=-99999
   17 CONTINUE
      IF(DR2.EQ.0.0)GO TO 30
   20 CONTINUE
C
   30 CONTINUE
C
      RETURN
      END
      SUBROUTINE ZRAY(A,N)
      DIMENSION A(N)
      DO I=1,N
        A(I)=0.
      END DO
      RETURN
      END
