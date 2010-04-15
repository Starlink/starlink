C+
C                               C O S R E J
C  Name:
C     COSREJ
C
C  Function:
C     Remove cosmic rays from a set of similar spectra.
C
C  Description:
C     Given an image whose cross-sections are a set of spectra of the
C     same object all with exactly the same wavelength to pixel mapping
C     (in other words a set of spectra that should be identical, other
C     than for signal to noise effects and possible differences in
C     their total counts due to differing exposure times etc), this
C     routine attempts to remove any cosmic rays or other obvious
C     features that might be contaminating some (but not the majority)
C     of the spectra.  First, the mean value for each spectrum over a
C     specified range is calculated and this is used to reduce each
C     spectrum to the same mean value.  Each pixel in each spectrum is
C     compared with the equivalent pixels in the other spectra, and any
C     that differ from the mean of the other pixels by more than a
C     given factor (the CRSIG parameter) times the standard deviation
C     of the other pixels are rejected.  This is repeated until either
C     only two pixels are left, or until no pixels are rejected in a
C     pass through the remaining pixels.  The rejected pixels are set
C     to the mean value of the remaining unrejected pixels.  Finally,
C     the spectra are rescaled to their original mean values.  If
C     requested, the program can create a `spectrum' with one element
C     for each spectrum in the original image, where each element is
C     the mean value used for that spectrum.  This might possibly be
C     used to scale the resulting image using, for example, ISYDIV.
C
C  Command parameters:
C     IMAGE    The name of the image in which the spectra are held.
C     XSTART   The first x-value of the range to be used to calculate
C              the mean value for each spectrum.
C     XEND     The last x-value of the range to be used to calculate
C              the mean value for each spectrum.
C     CRSIG    The cutoff sigma value to be used.
C     MSPECT   The name of the mean spectrum produced, if WMEAN is true.
C     OUTPUT   The name of the resulting image with the cosmic rays removed.
C
C  Command keywords:
C     WMEAN    True if a spectrum of mean values is to be produced.
C
C  Error array handling: Ignored.
C
C  Data quality / flagged value handling:
C     Not explicitly performed.  Relies on standard DSA processing.
C
C  Files used:
C     BADPIX.DAT  Contains a list of the cosmic rays removed from the data.
C
C  History:
C     8th  Sept  1987  Original version DJA / AAO
C     6th  March 1990  Reworked to use DSA routines, and some changes
C                      to the parameters made.  KS / AAO.
C     2nd  July  1990  Fixed 'double counting' bug causing excessive
C                      cosmic rays to be reported.
C     22nd Sep   1992  HME / UoE, Starlink. TABs removed. INCLUDE changed.
C                      Lowercase file name.
C     10th July  1995  HME / UoE, Starlink. Close loophole whereby
C                      less than 2 good pixels might remain in some
C                      columns. Once there would be less than 2, a
C                      division by zero would have occured.
C     2005 June 7      MJC / Starlink  Use CNF_PVAL for pointers to
C                      mapped data.
C+
      SUBROUTINE COSREJ
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      CHARACTER  ICH_CI*5
C
C     Maximum number of spectra to be analysed.  This must match the
C     value used by FIG_COS_REJ.
C
      INTEGER MAXSPEC
      PARAMETER (MAXSPEC=100)
C
C     Numeric parameter limits - close to VAX real limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     Local variables
C
      INTEGER   BFILE               ! Logical unit for list file, 0 if none
      REAL      CRSIG               ! Sigma value for rejection criterion
      INTEGER   DIMS(10)            ! The dimensions of the image data array
      INTEGER   DPTR                ! Dynamic memory element for image data
      LOGICAL   FAULT               ! True if any non-DSA error occurs
      INTEGER   IGNORE              ! Used to ignore status codes
      INTEGER   IXEN                ! Last x-element to be used
      INTEGER   IXST                ! First x-element to be used
      INTEGER   MPTR                ! Dynamic memory element for mean spectrum
      INTEGER   NDIM                ! Dimensionality of input data structure
      INTEGER   NELM                ! Total number of data elements - ignored
      INTEGER   NX                  ! Total number of elements per cross-section
      INTEGER   NY                  ! Total number of cross-sections
      INTEGER   REJ                 ! The number of pixels rejected
      INTEGER   SLOT                ! DSA system map slot - ignored
      INTEGER   STATUS              ! Inherited status value for DSA routines
      CHARACTER STRING*80           ! Message output text
      LOGICAL   WMEAN               ! True if 'means' spectrum to be produced
      REAL      XEN                 ! Last x-value in range to be used
      REAL      XST                 ! First x-value in range to be used
C
C     Initial values
C
      FAULT=.FALSE.
C
C     Initialise DSA system
C
      STATUS=0
      CALL DSA_OPEN (STATUS)
C
C     Open input image file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Get size of data in IMAGE and make sure we can handle the number
C     of spectra.
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error  exit
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('Input data is not an image',IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      NY=DIMS(2)
      NX=DIMS(1)
      IF (NY.LT.3) THEN
         CALL PAR_WRUSER('Image must have 3 or more spectra',IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
      IF (NY.GT.MAXSPEC) THEN
         CALL PAR_WRUSER('Image has too many spectra.'//
     :       ' This program can only handle '//ICH_CI(MAXSPEC),STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Put out a warning if there is 2D X-data.
C
      CALL DSA_AXIS_SIZE ('IMAGE',1,2,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.GT.1) THEN
         CALL PAR_WRUSER ('Warning: image has 2D x-axis data. All '//
     :         'spectra may not have same wavelength data.',IGNORE)
      END IF
C
C     Get the limits in X that we are to use
C
      CALL DSA_AXIS_RANGE ('IMAGE',1,' ',.FALSE.,XST,XEN,
     :                                               IXST,IXEN,STATUS)
C
C     Get value for CRSIG
C
      CALL PAR_RDVAL('CRSIG',1.0,FMAX,5.,' ',CRSIG)
C
C     Open the text file for the list of bad pixels.
C
      CALL DSA_GET_LU (BFILE,STATUS)
      OPEN (UNIT=BFILE,FILE='badpix.dat',STATUS='NEW',
     :                                FORM='FORMATTED',IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error opening bad pixel data file',IGNORE)
         CALL GEN_FORTERR (STATUS,.FALSE.,STRING)
         CALL PAR_WRUSER (STRING,IGNORE)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     See if we are to write out the weights array, and if so, open
C     the weights spectrum and create and map the main data array.
C
      CALL PAR_RDKEY ('WMEAN',.FALSE.,WMEAN)
      IF (WMEAN) THEN
         CALL DSA_OUTPUT ('MSPECT','MSPECT',' ',0,0,STATUS)
         CALL DSA_SIMPLE_OUTPUT ('MSPECT','DATA','FLOAT',1,NY,STATUS)
         CALL DSA_MAP_DATA ('MSPECT','WRITE','FLOAT',MPTR,SLOT,STATUS)
      END IF
C
C     Open the output image file.
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C     Map the data array
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',DPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Perform the rejection algorithm
C
      CALL FIG_COS_REJ(NX,NY,IXST,IXEN,CRSIG,WMEAN,BFILE,
     :                 %VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(MPTR)),REJ)
C
C     Exit and close everything down
C
  500 CONTINUE
      CALL DSA_CLOSE (STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_COS_REJ (NX,NY,IXST,IXEN,CRSIG,WMEAN,BFILE,DATA,
     :                                                     MSPEC,REJ)
C
C     F I G _ C O S R E J
C
C     Attempts to remove cosmic rays from an array containing a set of
C     similar spectra.  The algorithm is described in more detail at
C     the start of the main COSREJ routine.  Optionally, will return
C     an array giving the mean values for each spectrum within
C     specified limits.
C
C     Parameters - (">" input , "<" output)
C
C     (>) NX       (Integer) The number of data values per spectrum.
C     (>) NY       (Integer) The number of spectra.
C     (>) IXST     (Integer) The first pixel number to be used for calculating
C                  the mean values of the spectra.
C     (>) IXEN     (Integer) The last pixel number to be used for calculating
C                  the mean values of the spectra.
C     (>) CRSIG    (Real) Cutoff in standard deviations
C     (>) WMEAN    (Logical) True if mean values are to be copied into MSPEC.
C     (>) BFILE    (Integer) Logical unit for list of rejected pixels.
C     (!) DATA     (Real array (NX,NY)) Data values.
C     (<) MSPEC    (Real array (NY)) Mean value spectrum.
C     (<) REJ      (Integer) The number of rejected pixels.
C
C                                            DJA / AAO 8th Sept 1987.
C     Modified:
C
C     6th March 1990.  Reworked for different parameters and slightly
C                      modified operation of COSREJ.  KS/AAO.
C     2nd July  1990.  Fixed 'double counting' bug causing excessive
C                      cosmic rays to be reported. KS/AAO.
C     10th July 1995.  The DO WHILE loop tries to enforce NLEFT>=2,
C                      but within the loop several pixels might be
C                      removed in a single iteration. Before removing a
C                      pixel we must check NLEFT as well. HME/UoE, Starlink.
C+
      IMPLICIT NONE
C
C     Maximum number of spectra to be analysed - must match value in
C     main routine.
C
      INTEGER MAXSPEC
      PARAMETER (MAXSPEC=100)
C
C     Parameters
C
      LOGICAL WMEAN
      INTEGER NX, NY, IXST, IXEN, REJ, BFILE
      REAL    DATA(NX,NY),MSPEC(NY),CRSIG
C
C     Local variables
C
      REAL      FACTOR             ! Scaling factor for spectra
      REAL      FMEAN              ! Mean value for set of pixels
      REAL      FSIGMA             ! Sigma value for pixels
      REAL      FSUM               ! Total value for pixels
      INTEGER   IX                 ! Loop counter through pixels in spectrum
      INTEGER   IY                 ! Loop counter through spectra
      INTEGER   IGNORE             ! Used to pass ignorable status code
      INTEGER   NLEFT              ! Number of unrejected spectra
      INTEGER   NREJ               ! Number of pixels rejected in last pass
      INTEGER   PIXEL              ! Loop counter along spectra
      LOGICAL   REJECTED(MAXSPEC)  ! TRUE if a pixel has been rejected
      INTEGER   SPECT              ! Spectrum being considered
      CHARACTER STRING*64          ! Output message text
      REAL      TOTAL              ! Used for summing set of values
      REAL      MEANS(MAXSPEC)     ! Mean values for spectra
C
C     Initial values
C
      REJ=0
C
C     Let's not trust people not to change the value of MAXSPEC in
C     ALL the places it has to be changed..
C
      IF (NY.GT.MAXSPEC) THEN
         CALL PAR_WRUSER ('Too many spectra passed to FIG_COS_REJ.',
     :                                                       IGNORE)
         CALL PAR_WRUSER ('Program aborting.',IGNORE)
         GO TO 500      ! Error exit
      END IF
C
C     First we calculate the mean values for each spectrum within the
C     specified limits.
C
      DO IY=1,NY
         TOTAL=0.0
         DO IX=IXST,IXEN
            TOTAL=TOTAL+DATA(IX,IY)
         END DO
         MEANS(IY)=TOTAL/FLOAT(IXEN-IXST+1)
      END DO
C
C     Correct the data so that all the spectra have the same mean level.
C
      DO IY=1,NY
         FACTOR=1.0/MEANS(IY)
         DO IX=1,NX
            DATA(IX,IY)=DATA(IX,IY)*FACTOR
         END DO
      END DO
C
C     This is where we do the hard work. Go along the spectra, one pixel at a
C     time...
C
      DO PIXEL=1,NX
C
C        Reset the pixels-rejected array
C
         DO IY=1,NY
            REJECTED(IY)=.FALSE.
         END DO
C
C        This loop is repeated until it either fails to throw out any
C        pixels, or until there are only two left - in which case we
C        can't get any further.  NREJ is the number of pixels rejected
C        on the previous pass.
C
         NLEFT=NY
         NREJ=-1
         DO WHILE ((NREJ.NE.0).AND.(NLEFT.GT.2))
            NREJ=0
C
C           Take each pixel in the column in turn
C
            DO SPECT=1,NY
C
C              Calculate the mean and standard deviation for all the
C              other pixels in the column (excluding those rejected
C              on previous passes).
C
               FSUM=0.0
               DO IY=1,NY
                  IF ((IY.NE.SPECT).AND.(.NOT.REJECTED(IY))) THEN
                     FSUM=FSUM+DATA(PIXEL,IY)
                  END IF
               END DO
               FMEAN=FSUM/(NLEFT-1)
               FSIGMA=0.0
               DO IY=1,NY
                  IF ((IY.NE.SPECT).AND.(.NOT.REJECTED(IY))) THEN
                     FSIGMA=FSIGMA+(ABS(DATA(PIXEL,IY)-FMEAN)**2)
                  END IF
               END DO
               FSIGMA=SQRT(FSIGMA/(NLEFT-1))
C
C              Test to see if the current pixel is outside the permitted range
C              If it is then flag it as rejected.
C              But refuse rejection if it would leave less then 3 valid pixels.
C
               IF (ABS(DATA(PIXEL,SPECT)-FMEAN).GT.(CRSIG*FSIGMA)) THEN
                  IF (.NOT.REJECTED(SPECT).AND.NLEFT.GT.2) THEN
                     REJECTED(SPECT)=.TRUE.
                     NLEFT=NLEFT-1
                     REJ=REJ+1
                     NREJ=NREJ+1
                  END IF
               END IF
            END DO
         END DO
C
C        Set the rejected pixels to the mean value of the unrejected
C        pixels, and list them as well.
C
         TOTAL=0.0
         DO IY=1,NY
            IF (.NOT.REJECTED(IY)) TOTAL=TOTAL+DATA(PIXEL,IY)
         END DO
         IF (NLEFT.GT.0) TOTAL=TOTAL/FLOAT(NLEFT)
         DO IY=1,NY
            IF (REJECTED(IY)) THEN
               DATA(PIXEL,IY)=TOTAL
               WRITE (STRING,'(A,I5,A,I5)',IOSTAT=IGNORE)
     :               'Rejected pixel number ',PIXEL,' in spectrum ',IY
               CALL PAR_WRUSER (STRING,IGNORE)
               WRITE (BFILE,'(1X,A)',IOSTAT=IGNORE) STRING
            END IF
         END DO
      END DO
C
C     Rescale data to original mean levels
C
      DO IY=1,NY
         DO IX=1,NX
            DATA(IX,IY)=DATA(IX,IY)*MEANS(IY)
         END DO
      END DO
C
C     Copy mean values into the mean spectrum if required
C
      IF (WMEAN) THEN
         DO IY=1,NY
            MSPEC(IY)=MEANS(IY)
         END DO
      END IF
C
C     Summary
C
      STRING=' '
      CALL PAR_WRUSER (STRING,IGNORE)
      WRITE (BFILE,'(1X,A)',IOSTAT=IGNORE) STRING
      WRITE (STRING,'(A,I10)',IOSTAT=IGNORE)
     :                        'Total no. pixels rejected = ',REJ
      CALL PAR_WRUSER (STRING,IGNORE)
      WRITE (BFILE,'(1X,A)',IOSTAT=IGNORE) STRING
      CALL PAR_WRUSER (' ',IGNORE)
C
C     Exit
C
  500 CONTINUE
C
      END
