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
C     from the individual cross-correlation.  It also sums the individual
C     cross-correlations, and calculates the shift given by that summed
C     cross-correlation.  The idea is that the shift determined in this
C     way can then be applied using ISHIFT.
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
C    21st Jan 1991.  JMS / AAO. Changed maximum allowed dimensions of image
C                    to 2, and aborts if image is 1D. Added STATUS checks
C                    and PAR_ABORT calls to support user requested aborts.
C                    Modified calculation of SIGMA variable to handle 
C                    negative values. Also, modified FIG_CROSS to handle
C                    zero arrays.
C    14th Feb 1991.  JMS / AAO. Set minimum X axis range to two pixels.
C    25th Sep 1992.  HME / UoE, Starlink. INCLUDE changed. Call PAR_WRUSER
C                    rather than DSA_WRUSER, FIG_SETERR rather than SETERR,
C                    JTY_PEAKFIT rather than PEAKFIT.
C    30th Oct 1992.  HME / EIA, Starlink. Unused FAULT logical removed.
C    15th Feb 1996.  HME / UoE, Starlink. Change access for cross
C                    correlation from update to write.
C+
      IMPLICIT NONE
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER DYN_INCREMENT
      INTEGER DYN_ELEMENT
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      DOUBLE PRECISION A(5)! Parabola polynomial coefficients?
      INTEGER   ADDRESS   ! Virtual address for data array
      INTEGER   ARRAY0    ! Dynamic memory element for workspace
      INTEGER   ARRAY1    ! Dynamic memory element for workspace
      REAL      AVSHFT    ! Average shift of the individual cross-sections
      INTEGER   BDOUBP    ! Number of bytes per item of type 'FLOAT'
      INTEGER   BFLOAT    ! Number of bytes per item of type 'FLOAT'
      INTEGER   BYTES     ! Bytes required for an array
      REAL      CENTER    ! Centre of peak
      LOGICAL   CFIT      ! True if continuum fit used
      INTEGER   CFN       ! Pointer to workspace for the correlation function
      INTEGER   CFNSUM    ! Pointer to workspace for the correlation sum array
      INTEGER   CORPTR    ! Dynamic memory element for cross-corr. data
      INTEGER   DIMS(2)   ! Flat field dimensions
      LOGICAL   ERRFLG    ! True if error detected while fitting peak
      INTEGER   FLPTR     ! Dynamic memory element for flat field data
      INTEGER   FT0       ! Pointer to workspace for FT of first spectrum
      INTEGER   FT1       ! Pointer to workspace for FT of second spectrum
      INTEGER   FTFCN     ! Pointer to workspace for the correlation function
      INTEGER   IMPTR     ! Dynamic memory element for image data
      INTEGER   IXEN      ! Last AXIS(1) element to be used
      INTEGER   IXST      ! First AXIS(1) element to be used
      INTEGER   IY        ! Loop variable
      INTEGER   IYEN      ! Last AXIS(2) element to be used
      INTEGER   IYST      ! First AXIS(2) element to be used
      INTEGER   KZ(4)     ! Defines the cosine bell 
      LOGICAL   LOG       ! If true, the individual shifts will be logged 
      INTEGER   NDIM      ! Number of image dimensions
      INTEGER   NDIMI     ! Image dimensions
      INTEGER   NELM      ! Number of elements in image - ignored
      INTEGER   NEXT      ! Used to format user messages
      LOGICAL   NORM      ! True if the cross-corr fn to be normalised 
      INTEGER   NX        ! NX0 or the next highest integer power of 2
      INTEGER   NX0       ! No. of elements in the two spectra
      INTEGER   NXLEN     ! First dimension of image
      INTEGER   NY        ! Second dimension of image
      LOGICAL   RECORD    ! If true, the cross-corr. is recorded
      REAL      SHIFT     ! Shift of the peak of the corr fn from the zero point
      REAL      SIGMA     ! Average of individual shifts
      REAL      SIGSQ     ! Correlation sum
      REAL      SIZE      ! Length of array undergoing peak fitting
      INTEGER   SLOT      ! Slot number for mapped data - ignored
      INTEGER   SPTR      ! Dynamic memory element for spectrum data
      INTEGER   STATUS    ! Running status for DSA routines
      CHARACTER STRING*64 ! Used to format user messages
      REAL      SUMSH     ! Shift sum
      REAL      WIDTH     ! The width of the correlation function
      REAL      XEN       ! Last AXIS(1) value to be used
      REAL      XST       ! First AXIS(1) value to be used
      INTEGER   XV        ! Pointer to workspace for pixel values
      REAL      YEN       ! Last AXIS(2) value to be used
      REAL      YST       ! First AXIS(2) value to be used
      REAL      ZPC       ! % of spectrum covered at each end by cosine bell 

C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Dynamic memory common - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
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
      BDOUBP=DSA_TYPESIZE('DOUBLE',STATUS)
      CALL GEN_POWER2(NX0,NX)
      BYTES = 2*NX0*BFLOAT + 3*NX*BDOUBP + 3*NX*BFLOAT
      CALL DSA_GET_WORKSPACE(BYTES,ADDRESS,SLOT,STATUS)
      ARRAY0=DYN_ELEMENT(ADDRESS)
      IF(STATUS.NE.0)GOTO 500
C
C     Calculate values for pointers into work area.
C
      ARRAY1=ARRAY0+NX0*BFLOAT
      FT0=ARRAY1+NX0*BFLOAT
      FT1=FT0+NX*BDOUBP
      FTFCN=FT1+NX*BDOUBP
      XV=FTFCN+NX*BDOUBP
      CFN=XV+NX*BFLOAT
      CFNSUM=CFN+NX*BFLOAT
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
      CALL GEN_FILL(NX*BFLOAT,0,DYNAMIC_MEM(CFNSUM))
C
C     Go through all the selected cross-sections, calculating the
C     relative shifts and cross-correlations.
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,SLOT,STATUS)
      IMPTR=DYN_ELEMENT(ADDRESS)
      IMPTR=DYN_INCREMENT(IMPTR,'FLOAT',(IXST-1)+(IYST-1)*NXLEN)

      CALL DSA_MAP_DATA('FLAT','READ','FLOAT',ADDRESS,SLOT,STATUS)
      FLPTR=DYN_ELEMENT(ADDRESS)
      FLPTR=DYN_INCREMENT(FLPTR,'FLOAT',(IXST-1)+(IYST-1)*NXLEN)
      BYTES=NX0*BFLOAT
      IF(STATUS.NE.0)GOTO 500
        
  
      DO IY=IYST,IYEN
C
C        Read the spectrum and template data into the work arrays
C
         CALL GEN_MOVE(BYTES,DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(ARRAY0))
         CALL GEN_MOVE(BYTES,DYNAMIC_MEM(FLPTR),DYNAMIC_MEM(ARRAY1))

         IMPTR=DYN_INCREMENT(IMPTR,'FLOAT',NXLEN)
         FLPTR=DYN_INCREMENT(FLPTR,'FLOAT',NXLEN)
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
         CALL FIG_CROSS(DYNAMIC_MEM(ARRAY0),DYNAMIC_MEM(ARRAY1),NX0,NX,
     :                  CFIT,ZPC,KZ,NORM,DYNAMIC_MEM(FT0),
     :                  DYNAMIC_MEM(FT1),DYNAMIC_MEM(FTFCN),
     :                  DYNAMIC_MEM(XV),DYNAMIC_MEM(CFN),SHIFT,WIDTH)
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
         CALL GEN_ADDAF(NX,DYNAMIC_MEM(CFN),DYNAMIC_MEM(CFNSUM),
     :                  DYNAMIC_MEM(CFNSUM))
         SUMSH=SUMSH+SHIFT
         SIGSQ=SIGSQ+SHIFT*SHIFT
      END DO
C
C     Calculate the two values for the shift
C
      CALL GEN_NFILLF(NX,DYNAMIC_MEM(XV))
      CALL JTY_PEAKFIT(NX,DYNAMIC_MEM(XV),0.,FLOAT(NX),
     :             DYNAMIC_MEM(CFNSUM),CENTER,WIDTH,A,ERRFLG)
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
         CALL DSA_MAP_DATA('CORRL','WRITE','FLOAT',ADDRESS,SLOT,STATUS)
         CORPTR=DYN_ELEMENT(ADDRESS)
         BYTES=NX*BFLOAT
         IF(STATUS.NE.0)GOTO 500
         CALL GEN_MOVE(BYTES,DYNAMIC_MEM(CFNSUM),DYNAMIC_MEM(CORPTR))
      END IF
  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
