C+
      SUBROUTINE RED4_EMLT( STATUS )
C
C     E M L T
C
C     Figaro version of the original SDRSYS routine EMLT, which analyses
C     emission lines in a spectrum, fitting gaussians to the strongest
C     lines and logging their positions, widths and centers.  Optionally,
C     it will also give line centers using a centre of moment analysis,
C     and can also produce a synthetic spectrum generated from the
C     positions and widths of the located lines.  Note: Figaro and SDRSYS
C     differ in their pixel numbering, Figaro counting from 1 and SDRSYS
C     counting from 0, so there will be a discrepancy of 1 between the
C     output from the two versions for any pixel number values; wavelength
C     values produced by the two should be the same.
C
C     Parameters -
C
C     SPECTRUM    (Character) The name of the spectrum to be analysed.
C     XSTART      (Numeric) The first X-value to be used for the analysis.
C     XEND        (Numeric) The last X-value to be used for the analysis.
C     LINES       (Numeric) If LINES is zero, all lines that can be
C                 fitted are listed.  Otherwise, it gives the number of
C                 lines to be included in the analysis, starting with the
C                 strongest and cutting off the weaker lines.
C     FWHM        (Numeric) If non-zero, all lines fitted are constrained
C                 to a full width at half maximum of this value - in pixels.
C     OUTPUT      (Character) The name of any synthetic spectrum to be
C                 generated.
C
C     Keywords -
C
C     MOMENTS     If specified, a center of moment analysis is also performed
C                 on all lines found.
C     SYNTH       If specified, a synthetic spectrum is generated.
C
C                                                  KS / AAO  4th March 1988
C     Modified:
C
C     16th Aug 1988   Some output formatting problems corrected.  KS/AAO.
C     25th Jan 1989   Bug in dispersion calculation when only a subset of
C                     the data used now corrected.  KS/AAO.
C     29th Oct 1990   FIG_EMLT stripped off, and routine compiled /CHECK=ALL
C                     for test purposes.  SMB/ROE.
C     30th May 1991   Attempt to convert into ADAM A-task. Try adding
C                     a STATUS argument.  It worked!! Renamed RED4_EMLT
C                     and inserted into RED4 library.  SMB/ROE.
C     19TH Feb 1993   Conform to error strategy. PND/JAC.
C+
      IMPLICIT NONE
C
C     Functions used
C
      LOGICAL GEN_CHKNSF
      INTEGER ICH_LEN, DYN_ELEMENT, DYN_INCREMENT, DSA_TYPESIZE, CHR_LEN
C
C     Local variables
C
      INTEGER   ADDRESS        ! Used for memory addresses
      INTEGER   AXDIMS(6)      ! Axis array dimensions
      INTEGER   BINPTR         ! Dynamic memory element for workspace array
      INTEGER   BYTES          ! Number of bytes in one cross-section
      LOGICAL   CALIB          ! True if axis is calibrated (not just 1..NX)
      INTEGER   DIMS(6)        ! Data array dimensions
      INTEGER   DPTR           ! Dynamic memory element for spectral data
      DOUBLE PRECISION DUMMY   ! Dummy numeric argument
      REAL      FWHM           ! Value of FWHM parameter
      INTEGER   IGNORE         ! Returned status value - ignored
      INTEGER   IPTR           ! Dynamic memory element for input data
      INTEGER   ISPEC          ! Loop index through data cross-sections
      INTEGER   IXEN           ! Last spectral element to be used
      INTEGER   IXSPEC         ! Counts through axis cross-sections
      INTEGER   IXST           ! First spectral element to be used
      INTEGER   LINES          ! Value of LINES parameter
      INTEGER   LOGLU          ! Logical unit number for output file
      LOGICAL   MOMENTS        ! Value of MOMENTS keyword
      CHARACTER NAME*128       ! Full name of results file
      INTEGER   NASPEC         ! Number of cross-sections in axis data
      INTEGER   NAXDIM         ! Number of axis array dimensions
      INTEGER   NAXELM         ! Number of axis array elements
      INTEGER   NDIM           ! Number of data array dimensions
      INTEGER   NELM           ! Number of data array elements
      INTEGER   NOCENS         ! Moments keyword, numeric value for FIG_EMLT
      INTEGER   NSPECT         ! Number of spectra in data
      INTEGER   NX             ! First dimension of data array
      INTEGER   OPTR           ! Dynamic memory element for output data
      INTEGER   SLOT           ! Used for DSA_ slot values
      INTEGER   STATUS         ! Running status for DSA_ routines
      CHARACTER STRING*20      ! Used for formatting spectrum numbers
      INTEGER   STRPTR         ! Dynamic memory element for workspace array
      LOGICAL   SYNTH          ! Value of SYNTH keyword
      CHARACTER UNITS*10       ! Axis units
      REAL      VALUE          ! Real value of numeric parameter
      REAL      WEND           ! Last axis value - ignored
      INTEGER   WIDPTR         ! Dynamic memory element for workspace array
      INTEGER   WPTR           ! Dynamic memory element for workspace array
      REAL      WSTART         ! First axis value - ignored
      INTEGER   XBASE          ! Dynamic memory element for start of axis data
      LOGICAL   XEXIST         ! True if there is an axis data array
      INTEGER   XPTR           ! Dynamic memory element for axis data
C
C     Dynamic memory support - defines DYNAMIC_MEM
C
      INCLUDE 'ADAMERRS'
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'RED4_COMMON.INC'
C
C     Abort of the STATUS is not ADAM__OK on entry.
C
      IF ( STATUS .NE. ADAM__OK ) RETURN
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of input file and open it
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
C
C     Get data dimensions
C
      CALL DSA_DATA_SIZE ('SPECT',6,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
      NX = DIMS(1)
      NSPECT = NELM/NX
C
C     See if there is any axis data, and if so, get its size
C     and its units, and map it.
C
      CALL DSA_SEEK_AXIS ('SPECT',1,XEXIST,STATUS)
      IF (XEXIST) THEN
         CALL DSA_AXIS_SIZE ('SPECT',1,6,NAXDIM,AXDIMS,NAXELM,STATUS)
         IF (STATUS.NE.0) GO TO 500
         NASPEC = NAXELM / AXDIMS(1)
         CALL DSA_GET_AXIS_INFO ('SPECT',1,1,UNITS,0,DUMMY,STATUS)
         CALL DSA_MAP_AXIS_DATA ('SPECT',1,'READ','FLOAT',ADDRESS,
     :                                                     SLOT,STATUS)
         XPTR = DYN_ELEMENT (ADDRESS)
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     Get range of data to be examined.
C
      CALL DSA_AXIS_RANGE ('SPECT',1,' ',.FALSE.,WSTART,WEND,
     :                                            IXST,IXEN,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get the rest of the fitting parameters - MOMENTS, LINES, FWHM
C
      CALL PAR_RDVAL ('LINES',0.0,FLOAT(NX/5 + 1),0.0,' ',VALUE)
      LINES = INT(VALUE)
      CALL PAR_RDVAL ('FWHM',0.0,50.0,0.0,'Pixels',FWHM)
      CALL PAR_RDKEY ('MOMENTS',.FALSE.,MOMENTS)
      IF (MOMENTS) THEN
         NOCENS = 0
      ELSE
         NOCENS = 1
      END IF
C
C     See if a synthetic spectrum is required, and if so open it
C     and map its data.
C
      CALL PAR_RDKEY ('SYNTH',.FALSE.,SYNTH)
      IF (SYNTH) THEN
         CALL DSA_OUTPUT ('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
         CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',ADDRESS,
     :                                                 SLOT,STATUS)
         OPTR = DYN_ELEMENT (ADDRESS)
         IF (STATUS.NE.0) GO TO 500
      END IF
C
C     FIG_EMLT needs a certain amount of workspace.  Three real arrays,
C     all (NX/5 + 1) elements long are needed.
C
      CALL DSA_GET_WORK_ARRAY (3*(NX/5 +1),'FLOAT',ADDRESS,SLOT,STATUS)
      STRPTR = DYN_ELEMENT (ADDRESS)
      WIDPTR = DYN_INCREMENT (STRPTR,'FLOAT',(NX/5 + 1))
      BINPTR = DYN_INCREMENT (WIDPTR,'FLOAT',(NX/5 + 1))
C
C     We have to be a little bit careful about the data array we pass
C     to FIG_EMLT, since it modifies it in the course of its analysis.
C     If we are creating a synthetic spectrum, this is fine, since we
C     can just pass it the original contents of the output array (which
C     match the input), let it clobber it, and then let it write the
C     synthetic spectrum into it.  If we just have an input spectrum
C     to work with, however, we need to get workspace that we can copy
C     the data into before letting FIG_EMLT loose on it.  We also
C     need to map the input spectrum.
C
      IF (.NOT.SYNTH) THEN
         CALL DSA_MAP_DATA ('SPECT','READ','FLOAT',ADDRESS,
     :                                                 SLOT,STATUS)
         IPTR = DYN_ELEMENT (ADDRESS)
         CALL DSA_GET_WORK_ARRAY (NX,'FLOAT',ADDRESS,SLOT,STATUS)
         WPTR = DYN_ELEMENT (ADDRESS)
      END IF
C
C     Open an output file for the results
C
      CALL DSA_OPEN_TEXT_FILE ( CGS4_ENG(1:CHR_LEN(CGS4_ENG))//'emlt.lis',' ',
     :   'NEW',.TRUE.,LOGLU, NAME,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     See if we do have calibrated X-axis data
C
      CALIB = .FALSE.
      IF (XEXIST) CALIB = .NOT. GEN_CHKNSF (DYNAMIC_MEM(XPTR),NX)
C
C     Now we loop through all the spectra in the DATA.  If we are
C     producing a synthetic spectrum, we can just pass FIG_EMLT the
C     output array.  Otherwise, we copy the data into the work array
C     first.  If the data is wavelength calibrated, we have to run
C     through the wavelength array as well as the data array.
C
      CALL PAR_WRUSER(' ',STATUS)
      IXSPEC = 1
      XBASE = XPTR
      BYTES = NX * DSA_TYPESIZE ('FLOAT',STATUS)
      DO ISPEC=1,NSPECT
         IF (NSPECT.GT.1) THEN
            WRITE (STRING,*,IOSTAT=IGNORE) 'Spectrum ',ISPEC
            CALL PAR_WRUSER(STRING,IGNORE)
            WRITE (LOGLU,'(A)',IOSTAT=IGNORE) STRING
         END IF
         IF (SYNTH) THEN
            DPTR = OPTR
            OPTR = DYN_INCREMENT (OPTR,'FLOAT',NX)
         ELSE
            CALL GEN_MOVE (BYTES,DYNAMIC_MEM(IPTR),DYNAMIC_MEM(WPTR))
            IPTR = DYN_INCREMENT (IPTR,'FLOAT',NX)
            DPTR = WPTR
         END IF
         CALL FIG_EMLT (DYNAMIC_MEM(XPTR),DYNAMIC_MEM(DPTR),NX,IXST,
     :              IXEN,FWHM,NOCENS,LINES,DYNAMIC_MEM(STRPTR),
     :              DYNAMIC_MEM(WIDPTR),DYNAMIC_MEM(BINPTR),CALIB,
     :              SYNTH,UNITS,LOGLU)
         IF (CALIB) THEN
            IXSPEC = IXSPEC + 1
            IF (IXSPEC.GT.NASPEC) THEN
               IXSPEC = 1
               XPTR = XBASE
            ELSE
               XPTR = DYN_INCREMENT (XPTR,'FLOAT',NX)
            END IF
         END IF
      END DO
C
C     Let the user know the file being used.
C
      CALL PAR_WRUSER(' ',IGNORE)
      CALL PAR_WRUSER('Results written to '//NAME(:ICH_LEN(NAME)),
     :                                                      STATUS)
C
C     Tidy up
C
  500 CONTINUE
      CALL DSA_CLOSE (STATUS)
C
      END
