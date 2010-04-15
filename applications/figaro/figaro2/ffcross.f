C+
      SUBROUTINE FFCROSS
C
C     F F C R O S S
C
C     Main body of the Figaro FFCROSS function.  This is for use
C     with some flat fields (notably IPCS) where there may be a bodily
C     shift between the flat field and the data.  For each
C     cross-section in a given range, this routine calculates the
C     cross-correlation between the flat field and the data.  It then
C     calculates the average shift for each cross-section, as determined
C     from the individual cross-correlation.  It also sums the
C     individual cross-correlations, and calculates the shift given by
C     that summed cross-correlation.  The idea is that the shift
C     determined in this way can then be applied using ISHIFT.
C
C     Command parameters -
C
C     IMAGE       (Character) The IMAGE to be compared with
C                 the flat field.
C     FLAT        (Character) The FLAT field to be used.
C                 The FLAT and IMAGE data arrays should have the same
C                 dimensions.
C     YSTART      (Numeric) The first cross-section to be used.
C     YEND        (Numeric) The last cross-section to be used.
C     XSTART      (Numeric) Data with an AXIS(1) value less than XSTART
C                 will be ignored in the cross-correlation.
C     XEND        (Numeric) Data with an AXIS(1) value greater than XEND
C                 will also be ignored.  Note that these values are
C                 used to determine the channel numbers to be used
C                 for IMAGE and the same ones will be used for
C                 FLAT even if FLAT has a  different AXIS(1)
C                 structure.
C     CROSS       (Character) the name of the data structure to hold
C                 the cross-correlation, if it is to be saved.
C                 The file created will be cross.dst, and will look
C                 like an ordinary spectrum - ie can be plotted by
C                 SPLOT, etc.  CROSS is ignored if RECORD is not
C                 specified.
C
C     Command keywords -
C
C     RECORD      If specified, the summed cross-correlation of the two
C                 images will be recorded as a new data structure.
C     LOG         If specified, the individual shifts for each cross-
C                 section will be logged as they are calculated.
C
C     User variables used -
C
C     SHIFT       (Numeric) The relative shift of the two images as
C                 determined from the summed cross-correlation.
C     AVSHIFT     (Numeric) The average shift of the individual
C                 cross-sections.
C
C                                             KS / CIT 5th Oct 1983
C     Modified:
C
C     6th May 1985.  KS / AAO.  Modified to allow for addition of
C                    CFIT to parameters in call to FIG_CROSS.
C    21st Oct  1988. JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C    21st Jan 1991.  JMS / AAO. Changed maximum allowed dimensions of
C                    image to 2, and aborts if image is 1D. Added STATUS
C                    checks and PAR_ABORT calls to support
C                    user-requested aborts. Modified calculation of
C                    SIGMA variable to handle negative values. Also,
C                    modified FIG_CROSS to handle zero arrays.
C    14th Feb 1991.  JMS / AAO. Set minimum X axis range to two pixels.
C    25th Sep 1992.  HME / UoE, Starlink. INCLUDE changed. Call
C                    PAR_WRUSER rather than DSA_WRUSER, FIG_SETERR
C                    rather than SETERR, JTY_PEAKFIT rather than
C                    PEAKFIT.
C    30th Oct 1992.  HME / EIA, Starlink. Unused FAULT logical removed.
C    15th Feb 1996.  HME / UoE, Starlink. Change access for cross
C                    correlation from update to write.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      DOUBLE PRECISION A(5)      ! Parabola polynomial coefficients?
      INTEGER   ARRAY0           ! Dynamic-memory pointer for workspace
      INTEGER   ARRAY1           ! Dynamic-memory pointer for workspace
      REAL      AVSHFT           ! Average shift of the individual
                                 ! cross-sections
      INTEGER   BFLOAT           ! Number of bytes per item of type
                                 ! 'FLOAT'
      INTEGER   BYTES            ! Bytes required for an array
      REAL      CENTER           ! Centre of peak
      LOGICAL   CFIT             ! True if continuum fit used
      INTEGER   CFN              ! Pointer to workspace for the
                                 ! correlation function
      INTEGER   CFNSUM           ! Pointer to workspace for the
                                 ! correlation sum array
      INTEGER   CORPTR           ! Dynamic-memory pointer for
                                 ! cross-corr. data
      INTEGER   DIMS(2)          ! Flat field dimensions
      LOGICAL   ERRFLG           ! True if error detected while fitting
                                 ! peak
      INTEGER   FLPTR            ! Dynamic-memory pointer for flat field
                                 ! data
      INTEGER   FT0              ! Pointer to workspace for FT of first
                                 ! spectrum
      INTEGER   FT1              ! Pointer to workspace for FT of second
                                 ! spectrum
      INTEGER   FTFCN            ! Pointer to workspace for the
                                 ! correlation function
      INTEGER   IMPTR            ! Dynamic-memory pointer for image data
      LOGICAL   ISNEW            ! Is address new to CNF?
      LOGICAL   ISNEWF           ! Is IMPTR address new to CNF?
      LOGICAL   ISNEWI           ! Is FLPTR address new to CNF?
      INTEGER   IXEN             ! Last AXIS(1) element to be used
      INTEGER   IXST             ! First AXIS(1) element to be used
      INTEGER   IY               ! Loop variable
      INTEGER   IYEN             ! Last AXIS(2) element to be used
      INTEGER   IYST             ! First AXIS(2) element to be used
      INTEGER   KZ(4)            ! Defines the cosine bell
      LOGICAL   LOG              ! If true, the individual shifts will
                                 ! be logged
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NDIMI            ! Image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   NEXT             ! Used to format user messages
      LOGICAL   NORM             ! Cross-corr function to be normalised?
      INTEGER   NX               ! NX0 or the next highest integer power
                                 ! of 2
      INTEGER   NX0              ! No. of elements in the two spectra
      INTEGER   NXLEN            ! First dimension of image
      INTEGER   NY               ! Second dimension of image
      LOGICAL   PISNFL           ! Previous CNF pointer FLPTR new?
      LOGICAL   PISNIM           ! Previous CNF pointer IMPTR new?
      LOGICAL   RECORD           ! If true, the cross-corr. is recorded
      REAL      SHIFT            ! Shift of the peak of the corr fn from
                                 ! the zero point
      REAL      SIGMA            ! Average of individual shifts
      REAL      SIGSQ            ! Correlation sum
      REAL      SIZE             ! Length of array undergoing peak
                                 ! fitting
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines
      CHARACTER STRING*64        ! Used to format user messages
      REAL      SUMSH            ! Shift sum
      INTEGER   TPTR             ! Temp dynamic-memory pointer
      REAL      WIDTH            ! The width of the correlation function
      REAL      XEN              ! Last AXIS(1) value to be used
      REAL      XST              ! First AXIS(1) value to be used
      INTEGER   XV               ! Pointer to workspace for pixel values
      REAL      YEN              ! Last AXIS(2) value to be used
      REAL      YST              ! First AXIS(2) value to be used
      REAL      ZPC              ! % of spectrum covered at each end by
                                 ! cosine bell

C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Open IMAGE file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Ditto FLAT
C
      CALL DSA_INPUT ('FLAT','FLAT',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get sizes of both data arrays
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIMI,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GOTO 500
      IF (NDIMI.EQ.1) THEN
         CALL PAR_WRUSER('Image is not 2-dimensional',STATUS)
         GOTO 500
      END IF
C
      NXLEN=DIMS(1)
      NX0=DIMS(1)
      NY=DIMS(2)
C
      CALL DSA_DATA_SIZE ('FLAT',2,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GOTO 500
      IF (NDIM.EQ.1) THEN
         CALL PAR_WRUSER('Flat field is not 2-dimensional',STATUS)
         GOTO 500
      END IF
C
      CALL DSA_MATCH_SIZES('IMAGE','FLAT',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Find the range of axis data to be used and
C     then reset NX0 to reflect the length of the spectrum to be used.
C
      CALL DSA_AXIS_RANGE('IMAGE',2,' ',.FALSE.,YST,YEN,IYST,IYEN,
     :                     STATUS)
      CALL DSA_AXIS_RANGE('IMAGE',1,' ',.FALSE.,XST,XEN,IXST,IXEN,
     :                     STATUS)
C
C     Check that the length of the specified X axis range is less than or
C     equal to two pixels.
C
      IF ((IXEN-IXST).LE.2) THEN
         CALL PAR_WRUSER('The range you have specified is too small.'//
     :      ' Try again with different XSTART and XEND values.',STATUS)
         GOTO 500
      END IF
C
      NX0=IXEN-IXST+1
C
C     The cross-correlation needs a lot of workspace, so grab that now.
C
      BFLOAT=DSA_TYPESIZE('FLOAT',STATUS)
      CALL GEN_POWER2(NX0,NX)

      CALL DSA_GET_WORK_ARRAY(NX0,'FLOAT',ARRAY0,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX0,'FLOAT',ARRAY1,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',FT0,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',FT1,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',FTFCN,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',XV,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',CFN,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',CFNSUM,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Find out if the cross-correlation is to be recorded, and if so,
C     get the name of the output file.
C
      CALL PAR_RDKEY('RECORD',.FALSE.,RECORD)
      IF (PAR_ABORT()) GOTO 500
      IF (RECORD) THEN
         CALL DSA_OUTPUT('CORRL','CROSS',' ',NO_DATA,NEW_FILE,STATUS)
         IF(STATUS.NE.0)GOTO 500
      END IF
C
C     Are we to log the individual cross-section results?
C
      CALL PAR_RDKEY('LOG',.FALSE.,LOG)
      IF (PAR_ABORT()) GOTO 500
      IF (LOG) CALL PAR_WRUSER(' ',STATUS)
C
C     Zero out the shift sum variable, and the correlation sum array
C
      SUMSH=0.
      SIGSQ=0.
      CALL GEN_FILL(NX*BFLOAT,0,%VAL(CNF_PVAL(CFNSUM)))
C
C     Go through all the selected cross-sections, calculating the
C     relative shifts and cross-correlations.
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IMPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA('FLAT','READ','FLOAT',FLPTR,SLOT,STATUS)

      CALL DYN_INCAD(FLPTR,'FLOAT',(IXST-1)+(IYST-1)*NXLEN,TPTR,
     :               ISNEW,STATUS)
      BYTES=NX0*BFLOAT
      IF(STATUS.NE.0)GOTO 500

      PISNFL = .FALSE.
      PISNIM = .FALSE.
      DO IY=IYST,IYEN
C
C        Read the spectrum and template data into the work arrays.
C
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(IMPTR)),
     :                 %VAL(CNF_PVAL(ARRAY0)))
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(FLPTR)),
     :                 %VAL(CNF_PVAL(ARRAY1)))

         CALL DYN_INCAD(IMPTR,'FLOAT',NXLEN,TPTR,ISNEWI,STATUS)
         IF (ISNEWI) CALL CNF_UNREGP(IMPTR)
         IMPTR=TPTR
         PISNIM = ISNEWI

         CALL DYN_INCAD(FLPTR,'FLOAT',NXLEN,TPTR,ISNEWF,STATUS)
         IF (ISNEW) CALL CNF_UNREGP(FLPTR)
         FLPTR=TPTR
         PISNFL = ISNEWF
C
C        Pick reasonable values for the fourier domain filter - this
C        section could be refined, but these will do..
C
         CFIT=.TRUE.
         ZPC=10.
         KZ(1)=5
         KZ(2)=MIN(20,NX-2)
         KZ(3)=MAX(NX/6,KZ(2)+1)
         KZ(4)=MIN(2*KZ(3),NX)
C
C        Perform the cross-correlation
C
         NORM=.FALSE.
         CALL FIG_CROSS(%VAL(CNF_PVAL(ARRAY0)),%VAL(CNF_PVAL(ARRAY1)),
     :                  NX0,NX,CFIT,ZPC,KZ,NORM,%VAL(CNF_PVAL(FT0)),
     :                  %VAL(CNF_PVAL(FT1)),%VAL(CNF_PVAL(FTFCN)),
     :                  %VAL(CNF_PVAL(XV)),%VAL(CNF_PVAL(CFN)),SHIFT,
     :                  WIDTH)
C
C        Log the result, if LOG was specified
C
         IF (LOG) THEN
            WRITE (STRING,'(A,I5,A,G10.3)',IOSTAT=STATUS)
     :              'Cross-section ',IY,', Shift = ',SHIFT
            CALL PAR_WRUSER(STRING,STATUS)
         END IF
C
C        Add this result into what we have already
C
         CALL GEN_ADDAF(NX,%VAL(CNF_PVAL(CFN)),%VAL(CNF_PVAL(CFNSUM)),
     :                  %VAL(CNF_PVAL(CFNSUM)))
         SUMSH=SUMSH+SHIFT
         SIGSQ=SIGSQ+SHIFT*SHIFT
      END DO

      IF (ISNEWI) CALL CNF_UNREGP(IMPTR)
      IF (ISNEWF) CALL CNF_UNREGP(FLPTR)
C
C     Calculate the two values for the shift
C
      CALL GEN_NFILLF(NX,%VAL(CNF_PVAL(XV)))
      CALL JTY_PEAKFIT(NX,%VAL(CNF_PVAL(XV)),0.,FLOAT(NX),
     :                 %VAL(CNF_PVAL(CFNSUM)),CENTER,WIDTH,A,ERRFLG)
      IF (ERRFLG) THEN
         CALL PAR_WRUSER(
     :      'Warning - error detected while fitting peak',STATUS)
      END IF

      SHIFT=FLOAT(NX/2)-CENTER+1.
      SIZE=FLOAT(IYEN-IYST+1)
      AVSHFT=SUMSH/SIZE
      IF (IYEN.GT.IYST) THEN
         SIGMA=SQRT(abs((SIGSQ-(SUMSH*SUMSH)/SIZE))/(SIZE-1.))
      END IF
C
C     And output the two values
C
      STRING='Average of individual shifts is '
      CALL ICH_ENCODE(STRING,AVSHFT,33,3,NEXT)
      IF (IYEN.GT.IYST) THEN
         STRING(NEXT:)=' elements,  +/- '
         CALL ICH_ENCODE(STRING,SIGMA,NEXT+16,3,NEXT)
      ELSE
         STRING(NEXT:)=' elements'
      END IF
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER(STRING,STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      STRING='Shift given by summed cross-correlation is '
      CALL ICH_ENCODE(STRING,SHIFT,44,3,NEXT)
      STRING(NEXT:)=' elements'
      CALL PAR_WRUSER(STRING,STATUS)
      CALL PAR_WRUSER(' ',STATUS)
C
C     Set the user variables SHIFT and AVSHIFT
C
      CALL VAR_SETNUM('SHIFT',0,0,SHIFT,STATUS)
      CALL VAR_SETNUM('AVSHIFT',0,0,AVSHFT,STATUS)
C
C     Now, if required, create the output structure for the
C     cross-correlation.
C
      IF (RECORD) THEN
         CALL DSA_COERCE_DATA_ARRAY('CORRL','FLOAT',1,NX,STATUS)
         CALL DSA_MAP_DATA('CORRL','WRITE','FLOAT',CORPTR,SLOT,STATUS)
         BYTES=NX*BFLOAT
         IF(STATUS.NE.0)GOTO 500
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(CFNSUM)),
     :                 %VAL(CNF_PVAL(CORPTR)))
      END IF
  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
