      SUBROUTINE MOMENTS
C+
C
C   -------------
C   M O M E N T S
C   -------------
C
C   Description
C   -----------
C   Calculates the moments (total flux, position, variance, skewness,
C   and kurtosis) of spectral emission components in a ZXY-sorted image.
C   The spectra may be binned in the spatial directions to improve
C   signal/noise. A mask image may be used to avoid parts of the image.
C   The moments results are written to a results structure.
C
C
C   Scope of program
C   ----------------
C   - Specific to three dimensions.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting not supported.
C   - Magic values supported.
C   - Quality and variance arrays supported.
C   - Batch execution supported.
C
C
C   Environment
C   -----------
C   FIGARO
C
C
C   Parameters (read or written)
C   ----------------------------
C   IMAGE    Name of the structure containing the image. (character)
C            (prompted for).
C
C   MASK     Name of the structure containing the mask image. (character)
C            (prompted for).
C
C   PEAKS    Number of peaks to be found. (integer)(prompted for).
C
C   THRESH   Threshold pixel value for peak search. (real)(prompted for).
C
C   WIDTH    Width of peaks in pixels. (real)(prompted for).
C
C   GAP      Minimum separation of peaks. (real)(prompted for).
C
C   BIN      Binning factor in the XY plane. (integer)(prompted for).
C
C   RESULTS  Name of the results structure. (character)(prompted for).
C
C
C   Keywords
C   --------
C   USEMASK  If specified, a mask image is used to select spectra.
C
C   FINDSEQ  If specified, the spectrum will be searched from the first pixel
C            and peaks will be processed in the sequence in which they are
C            encountered. A peak is identified if the flux rises to a maximum
C            and then drops below THRESH. The WIDTH of peaks and the GAP
C            between them will be used to avoid areas where valid peaks are not
C            likely to be found. If FINDSEQ is not specified, peaks will be
C            located by finding the maximum pixel in the spectrum. A number of
C            pixels around the peak, governed by WIDTH, will then be masked and
C            the next maximum will be found among the pixels remaining.
C            (hidden keyword).
C
C   WEIGHT   Flag to determine whether to weight statistics with variance
C            values (if applicable).
C
C   Propagation of data structure
C   -----------------------------
C   Not relevant.
C
C
C   Method
C   ------
C   - The IMAGE structure is tested for the bad data flag, a quality array
C     and an error array. If no quality array is present, and the bad pixel
C     value is non-zero, magic values are assumed to be present and are left in
C     the data. If a quality array is present, the DSA routines are used
C     to convert these to equivalent magic values.
C   - If a mask image is requested, the dimensions of its data array must
C     match the dimensions of IMAGE which have been designated X and Y, in
C     this case dimensions 2 and 3 since IMAGE is ZXY-sorted.
C   - The results structure for MOMENTS is created.
C   - The IMAGE data array may be binned in the spatial dimensions to
C     improve signal/noise. The bin average is computed at each pixel in the
C     spectral dimension. MOMENTS_MASKBIN performs binning and masking. If
C     masking is requested, a spectrum only contributes to the average in
C     a bin if the relevant MASK pixel is clear, i.e. non-magic. A 3-D work
C     array containing the binned and/or masked data is used in subsequent
C     operations. If the binning interval is 1 and a mask is not used,
C     MOMENTS_MASKBIN is skipped and the IMAGE array is mapped directly.
C   - Once the masking has been done, the MASK array is unmapped and the
C     structure is closed. This is because the maximum number of structures
C     which may be referenced at one time is eight, set by the MAX_REFS
C     parameter in the DSA common block.
C   - Peaks may be found in two ways. MOMENTS_FINDMAX locates a peak by
C     looking for the maximum pixel over the whole spectrum, regardless of
C     where it is. MOMENTS_FINDSEQ starts at pixel 1 and identifies a peak
C     if the flux rises to a maximum and then drops below the required
C     threshold. The first method is more commonly used, but if there are
C     two or more emissions of more or less equal strength, the second
C     delivers them in a definite order. Once a peak has been found, both
C     routines store the pixel number of its maximum value in a 2-D work
C     array, and then mask the required width of spectrum around the peak by
C     flagging the corresponding pixels in a 3-D work array.
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_DATA_SIZE
C     DSA_GET_WORKSPACE
C     DSA_INPUT
C     DSA_MAP_AXIS_DATA
C     DSA_MAP_DATA
C     DSA_MAP_VARIANCE
C     DSA_OPEN
C     DSA_SEEK_ERRORS
C     DSA_SEEK_QUALITY
C     DSA_SIMPLE_OUTPUT
C     DSA_TYPESIZE
C     DSA_UNMAP
C     DSA_USE_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library GEN:
C     GEN_FILL
C
C   Library ICH:
C     ICH_CI
C     ICH_ENCODE
C     ICH_LEN
C
C   Library NDP:
C     NDP_GET_IMAGE_INFO
C     NDP_MATCH_SIZES
C     NDP_SET_BAD_PIXEL
C
C   Library PAR:
C     PAR_RDCHAR
C     PAR_RDKEY
C
C   Library VAR:
C     VAR_GETNUM
C     VAR_SETNUM
C
C
C   Internal subroutines called
C   ---------------------------
C     MOMENTS_MASKBIN_W
C     MOMENTS_MASKBIN_WQ
C     MOMENTS_MASKBIN_R
C     MOMENTS_MASKBIN_RQ
C     MOMENTS_COMPUTE_R
C     MOMENTS_COMPUTE_RQ
C     MOMENTS_COMPUTE_RV
C     MOMENTS_COMPUTE_RQV
C     MOMENTS_FINDMAX_R
C     MOMENTS_FINDMAX_RQ
C     MOMENTS_FINDSEQ_R
C     MOMENTS_FINDSEQ_RQ
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   03-AUG-1990   - Fixed so that adjustable array sizes are not passed
C                   as elements of another array.  This practice has been
C                   banned by the v5.2 compiler.  (JRL)
C   13-NOV-1991   - Oh my god what a mess... I cleared up the messy results
C                   structure, replaced it with a much simpler animal created
C                   using DSA_SIMPLE_OUTPUT. Added
C                   the usual quality and error array processing. (GOLDJIL)
C
C   16-MAR-1992   - Got rid of DSA_ACTNAME and replaced DSA_CFILLx's with
C                   GEN_FILL's.
C
C   02-DEC-1992   - Unix version.
C   06-OCT-1994   - Unused variables removed. (GJP)
C   10-OCT-1994   - Added section setting up workspace for
C                   component count (suggested by JRL) (GJP)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C   Functions used.
C
      CHARACTER ICH_CI*8
      INTEGER   DSA_TYPESIZE,DYN_ELEMENT,ICH_ENCODE,ICH_LEN
C
C   Local variables.
C
      CHARACTER ACT_NAME*128      ! Actual full structure name
      INTEGER   ADDRESS           ! Address of dynamic memory element
      INTEGER   AXPTR             ! Dynamic pointer to spectral axis
      INTEGER   AXSLOT            ! Map slot number for spectral axis
      LOGICAL   BADPIX            ! Value of bad pixel flag in IMAGE
      INTEGER   BIN               ! Binning factor for image pixels
      INTEGER   BINDIMS(10)       ! Dimensions of binned arrays
      INTEGER   BINNELM           ! Number of elements in binned arrays
      INTEGER   BINPTR            ! Dynamic pointer to binned data array
      INTEGER   BINSLOT           ! Map slot number for binned data array
      INTEGER   BYTES             ! How many bytes does sir/madam require?
      CHARACTER CHCOMP            ! Component number as character string
      INTEGER   CMPPTR            ! Dynamic pointer to component count array
      INTEGER   CMPSLOT           ! Map slot number of count array
      INTEGER   DIMS(10)          ! Dimensions of IMAGE
      INTEGER   DUMINT            ! INTEGER dummy variable
      REAL      DUMREAL           ! REAL dummy variable
      INTEGER   ELEM              ! Element size in bytes
      LOGICAL   ERR               ! Flags presence of error array in IMAGE
      LOGICAL   FINDMAX           ! Instruction to find peaks at overall maxima
      LOGICAL   FINDSEQ           ! Instruction to find peaks in sequence
      REAL      GAP               ! Minimum separation of peaks in pixels
      INTEGER   I                 ! Loop counter
      INTEGER   ICOMP             ! Current component substructure
      INTEGER   IMPTR             ! Dynamic pointer to IMAGE data
      INTEGER   ISLOT             ! Map slot number for IMAGE data
      INTEGER   IVPTR             ! IMAGE variance array pointer thingy
      INTEGER   IVSLOT            ! Slot number for above
      INTEGER   MSKPTR            ! Dynamic pointer to mask array
      INTEGER   MSKSLOT           ! Map slot number for mask array
      CHARACTER MSKFILE*128       ! Mask file name
      INTEGER   NDIM              ! Number of dimensions in IMAGE
      INTEGER   NELM              ! Number of elements in IMAGE
      INTEGER   NEXT              ! Pointer returned by ICH_ENCODE
      INTEGER   NPAR              ! Number of parameters
      CHARACTER OPTIONS*64        ! DSA_SIMPLE_OUTPUT item list
      INTEGER   PEAKS             ! Number of peaks to be found
      INTEGER   PKPTR             ! Dynamic pointer to peak position array
      INTEGER   PKSLOT            ! Map slot number for peak position array
      LOGICAL   QUAL              ! Flags presence of quality array in IMAGE
      INTEGER   RESDIMS(4)        ! Contains dimensions of results structure
      INTEGER   RPTR              ! Dynamic pointer to RESULTS data array
      INTEGER   RSLOT             ! Map slot number for RESULTS data array
      INTEGER   RVPTR             ! Dynamic pointer to RESULTS variance array
      INTEGER   RVSLOT            ! Map slot number for RESULTS variance array
      INTEGER   SPPTR             ! Dynamic pointer to spectrum array
      INTEGER   SPSLOT            ! Map slot number for spectrum array
      INTEGER   STATUS            ! Status code
      CHARACTER STRING*80         ! Message string
      REAL      THRESH            ! Threshold pixel value for peak search
      CHARACTER TITLE(5)*64       ! Titles for parameter arrays
      CHARACTER TYPE*8            ! IMAGE data array type
      LOGICAL   USEMASK           ! Instruction to use a mask image
      INTEGER   USPTR             ! Dynamic pointer to pixel usage array
      INTEGER   USSLOT            ! Map slot number for pixel usage array
      INTEGER   VALPTR            ! Dynamic pointer to validity array
      INTEGER   VALSLOT           ! Map slot number for validity array
      INTEGER   VBINPTR           ! Dynamic pointer to variance bin array
      INTEGER   VBINSLOT          ! Map slot number for variance bin array
      LOGICAL   WEIGHT            ! Use normal or weighted means?
      REAL      WIDTH             ! Width of peak in pixels
      INTEGER   WRKPTR            ! Dynamic pointer to workspace array
      INTEGER   WRKSLOT           ! Map slot number for workspace array
      INTEGER   XYAXES(2)         ! Image axes which represent X and Y dims
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
      PARAMETER (NPAR=5)
C
C   Initialize.
C
      XYAXES(1)=2
      XYAXES(2)=3
      TITLE(1)='Total flux (zeroth moment)'
      TITLE(2)='Peak position (first moment)'
      TITLE(3)='Variance (second moment)'
      TITLE(4)='Skewness (third moment)'
      TITLE(5)='Kurtosis (fourth moment)'
C
C   Open DSA system.
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Open file for IMAGE.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get information about IMAGE, which must be ZXY-sorted. The user must verify
C   this by looking at the output from NDP_GET_IMAGE_INFO.
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.FALSE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get dimensions of IMAGE data array.
C
      CALL DSA_DATA_SIZE('IMAGE',3,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
      IF(NDIM.LT.3)THEN
        CALL DSA_WRUSER('This is not a 3-D image.\\N')
        GO TO 500
      END IF
C
C   Find out about error/quality information
C
      CALL DSA_SEEK_ERRORS('IMAGE',ERR,STATUS)
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C   Use magic values even if quality is present
C
      IF (QUAL) BADPIX=.TRUE.
      IF (ERR)
     &  CALL PAR_RDKEY('WEIGHT',.TRUE.,WEIGHT)
C
C   Get instruction to use a mask image.
C
      CALL PAR_RDKEY('USEMASK',.FALSE.,USEMASK)
C
C   Open file for mask image and check that its dimensions match those of the
C   image to be processed.
C
      IF(USEMASK)THEN
        CALL PAR_RDCHAR('MASK',' ',MSKFILE)
c       CALL DSA_ACTNAME(MSKFILE,' ',ACT_NAME,STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_NAMED_INPUT('MASK',ACT_NAME,STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL NDP_MATCH_SIZES('MASK','IMAGE',XYAXES,STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C   Get instruction to find peaks at overall maxima.
C
      CALL PAR_RDKEY('FINDSEQ',.FALSE.,FINDSEQ)
      FINDMAX=.NOT.FINDSEQ
C
C   Get number of peaks to be found.
C
      CALL PAR_RDVAL('PEAKS',1.0,8.0,1.0,' ',DUMREAL)
      PEAKS=INT(DUMREAL)
C
C   Get threshold pixel value.
C
      CALL PAR_RDVAL('THRESH',0.0,MAX_FLOAT,100.0,' ',THRESH)
C
C   Get peak width.
C
      CALL PAR_RDVAL('WIDTH',0.0,MAX_FLOAT,10.0,' ',WIDTH)
C
C   Get peak separation if finding peaks in sequence from pixel 1.
C
      IF(FINDSEQ)THEN
        CALL PAR_RDVAL('GAP',0.0,MAX_FLOAT,10.0,' ',GAP)
      END IF
C
C   Get binning factor.
C
      CALL PAR_RDVAL('BIN',1.0,180.0,1.0,' ',DUMREAL)
      BIN=INT(DUMREAL)
C
C   Compute dimensions and number of elements in binned data array.
C
      BINDIMS(1)=DIMS(1)
      BINDIMS(2)=DIMS(2)/BIN
      BINDIMS(3)=DIMS(3)/BIN
      BINNELM=1
      DO I=1,3
        BINNELM=BINNELM*BINDIMS(I)
      END DO
C
C   Open file for results structure.
C
      CALL DSA_OUTPUT('RESULTS','RESULTS',' ',1,1,STATUS)
      IF (STATUS.NE.0) GO TO 500
      OPTIONS='D,E,'
      DO I=3,4
        OPTIONS=OPTIONS//'A'//ICH_CI(I)//'['//ICH_CI(DIMS(I-1))//'],'
      END DO
      RESDIMS(1)=NPAR
      RESDIMS(2)=PEAKS
      RESDIMS(3)=DIMS(2)
      RESDIMS(4)=DIMS(3)
      CALL DSA_SIMPLE_OUTPUT('RESULTS',OPTIONS,'FLOAT',4,RESDIMS,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C   Map IMAGE data array. If the image is to be masked or binned, map it in its
C   own data type. The MOMENTS_MASKBIN_* routine will transfer it to a REAL
C   array. If neither masking nor binning is required, map the data array as
C   FLOAT and assign it the BINPTR pointer, so as to pretend that it has been
C   through the masking/binning routine.
C
      CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
      IF(USEMASK .OR. BIN.GT.1)THEN
        IF(TYPE.EQ.'SHORT')THEN
          CALL DSA_MAP_DATA('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          IMPTR=DYN_ELEMENT(ADDRESS)
          IF (ERR) THEN
            CALL DSA_MAP_VARIANCE('IMAGE','READ','SHORT',
     &                            ADDRESS,IVSLOT,STATUS)
            IF(STATUS.NE.0)GO TO 500
            IVPTR=DYN_ELEMENT(ADDRESS)
          END IF
        ELSE
          CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          IMPTR=DYN_ELEMENT(ADDRESS)
          IF (ERR) THEN
            CALL DSA_MAP_VARIANCE('IMAGE','READ','FLOAT',
     &                              ADDRESS,IVSLOT,STATUS)
            IF(STATUS.NE.0)GO TO 500
            IVPTR=DYN_ELEMENT(ADDRESS)
          END IF
        END IF
      ELSE
        CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        BINPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) THEN
          CALL DSA_MAP_VARIANCE('IMAGE','READ','FLOAT',
     &                            ADDRESS,IVSLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          IVPTR=DYN_ELEMENT(ADDRESS)
        END IF
      END IF
C
C   Map the spectral axis of IMAGE.
C
      CALL DSA_MAP_AXIS_DATA
     &  ('IMAGE',1,'READ','FLOAT',ADDRESS,AXSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      AXPTR=DYN_ELEMENT(ADDRESS)
C
C   Get workspace for binned data array. This 3-D REAL array contains the image
C   data after it has been rebinned in X and Y.
C
      IF(BIN.GT.1)THEN
        ELEM=DSA_TYPESIZE('FLOAT',STATUS)
        CALL DSA_GET_WORKSPACE(BINNELM*ELEM,ADDRESS,BINSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        BINPTR=DYN_ELEMENT(ADDRESS)
C
C   Do same for the variance array if it exists
C
        IF (ERR) THEN
          CALL DSA_GET_WORKSPACE(BINNELM*ELEM,ADDRESS,VBINSLOT,STATUS)
          IF (STATUS.NE.0) GO TO 500
          VBINPTR=DYN_ELEMENT(ADDRESS)
        END IF
      END IF
C
C   Get workspace for peak position array and initialize it with zeros. This
C   2-D INTEGER array contains the pixel number at which the current peak is
C   found.
C
      ELEM=DSA_TYPESIZE('INT',STATUS)
      CALL DSA_GET_WORKSPACE
     &  (BINDIMS(2)*BINDIMS(3)*ELEM,ADDRESS,PKSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      PKPTR=DYN_ELEMENT(ADDRESS)
      BYTES=BINDIMS(2)*BINDIMS(3)*DSA_TYPESIZE('INT',STATUS)
      CALL GEN_FILL(BYTES,0,DYNAMIC_MEM(PKPTR))
C
C   Get workspace for usage array and initialize it with zeros. This 3-D BYTE
C   array is a mask indicating which pixels in each spectrum have been used in
C   moments computation. It is only used when peaks are to be found at overall
C   maxima.
C
      IF(FINDMAX)THEN
        ELEM=DSA_TYPESIZE('BYTE',STATUS)
        CALL DSA_GET_WORKSPACE
     &    (BINDIMS(1)*BINDIMS(2)*BINDIMS(3)*ELEM,ADDRESS,USSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        USPTR=DYN_ELEMENT(ADDRESS)
        BYTES=BINDIMS(1)*BINDIMS(2)*BINDIMS(3)
        CALL GEN_FILL(BYTES,0,DYNAMIC_MEM(USPTR))
      END IF
C
C   Get workspace for spectrum array. This 1-D REAL array contains the spectrum
C   or average binned spectrum from which moments will be computed.
C
      ELEM=DSA_TYPESIZE('FLOAT',STATUS)
      CALL DSA_GET_WORKSPACE(DIMS(1)*ELEM,ADDRESS,SPSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      SPPTR=DYN_ELEMENT(ADDRESS)
C
C   Get workspace for validity array. This 1-D INTEGER*2 array flags the valid,
C   i.e. not magic value, pixels in the spectrum array. It is used in the
C   computation of the average value at each binned pixel.
C
      ELEM=DSA_TYPESIZE('SHORT',STATUS)
      CALL DSA_GET_WORKSPACE(DIMS(1)*ELEM,ADDRESS,VALSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      VALPTR=DYN_ELEMENT(ADDRESS)
C
C   Get workspace for masking/binning routine
C
      ELEM=DSA_TYPESIZE('FLOAT',STATUS)
      CALL DSA_GET_WORKSPACE(DIMS(1)*ELEM,ADDRESS,WRKSLOT,STATUS)
      IF (STATUS.NE.0)GO TO 500
      WRKPTR=DYN_ELEMENT(ADDRESS)

C
C   Get workspace for component count
C
      ELEM=DSA_TYPESIZE('BYTE',STATUS)
      CALL DSA_GET_WORKSPACE(DIMS(2)*DIMS(3)*ELEM,ADDRESS,
     : CMPSLOT,STATUS)
      IF (STATUS.NE.0)GO TO 500
      CMPPTR=DYN_ELEMENT(ADDRESS)
      CALL GEN_FILL(DIMS(2)*DIMS(3)*ELEM,0,DYNAMIC_MEM(CMPPTR))

C
C   If a mask is being used, map its data array as FLOAT (this covers all
C   likely types of mask). Otherwise, map 4 bytes of dummy workspace.
C
      IF(USEMASK)THEN
        CALL DSA_USE_FLAGGED_VALUES('MASK',STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_MAP_DATA('MASK','READ','FLOAT',ADDRESS,MSKSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        MSKPTR=DYN_ELEMENT(ADDRESS)
      ELSE
        ELEM=DSA_TYPESIZE('FLOAT',STATUS)
        CALL DSA_GET_WORKSPACE(ELEM,ADDRESS,MSKSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        MSKPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C   Mask and/or bin the image data and variance arrays if required.
C
      IF(USEMASK .OR. BIN.GT.1)THEN
        CALL DSA_WRUSER('Masking and/or binning arrays...\\N')
        IF(TYPE.EQ.'SHORT')THEN
          IF(.NOT.BADPIX)THEN
            CALL MOMENTS_MASKBIN_W
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(MSKPTR),
     &         DYNAMIC_MEM(SPPTR),DYNAMIC_MEM(VALPTR),
     &         DYNAMIC_MEM(BINPTR),ERR,DYNAMIC_MEM(IVPTR),
     &         DYNAMIC_MEM(VBINPTR),DIMS(1),DIMS(2),DIMS(3),
     &         BINDIMS(1),BINDIMS(2),BINDIMS(3),
     &         BIN,USEMASK,MAGIC_FLOAT,DYNAMIC_MEM(WRKPTR))
          ELSE
            CALL MOMENTS_MASKBIN_WQ
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(MSKPTR),
     &         DYNAMIC_MEM(SPPTR),DYNAMIC_MEM(VALPTR),
     &         DYNAMIC_MEM(BINPTR),ERR,DYNAMIC_MEM(IVPTR),
     &         DYNAMIC_MEM(VBINPTR),DIMS(1),DIMS(2),DIMS(3),
     &         BINDIMS(1),BINDIMS(2),BINDIMS(3),
     &         BIN,USEMASK,MAGIC_FLOAT,MAGIC_SHORT,DYNAMIC_MEM(WRKPTR))
          END IF
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL MOMENTS_MASKBIN_R
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(MSKPTR),
     &         DYNAMIC_MEM(SPPTR),DYNAMIC_MEM(VALPTR),
     &         DYNAMIC_MEM(BINPTR),ERR,DYNAMIC_MEM(IVPTR),
     &         DYNAMIC_MEM(VBINPTR),DIMS(1),DIMS(2),DIMS(3),
     &         BINDIMS(1),BINDIMS(2),BINDIMS(3),
     &         BIN,USEMASK,MAGIC_FLOAT,DUMREAL,DYNAMIC_MEM(WRKPTR))
          ELSE
            CALL MOMENTS_MASKBIN_RQ
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(MSKPTR),
     &         DYNAMIC_MEM(SPPTR),DYNAMIC_MEM(VALPTR),
     &         DYNAMIC_MEM(BINPTR),ERR,DYNAMIC_MEM(IVPTR),
     &         DYNAMIC_MEM(VBINPTR),DIMS(1),DIMS(2),DIMS(3),
     &         BINDIMS(1),BINDIMS(2),BINDIMS(3),
     &         BIN,USEMASK,MAGIC_FLOAT,MAGIC_FLOAT,DYNAMIC_MEM(WRKPTR))
          END IF
        END IF
      END IF
C
C   Unmap the mask array and close the structure, since it is no longer needed.
C   This frees a reference name slot for later use.
C
      IF(USEMASK)THEN
        CALL DSA_UNMAP(MSKSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_CLOSE_STRUCTURE('MASK',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C   Map the parameter data array and variance array
C
        CALL DSA_MAP_DATA('RESULTS','UPDATE','FLOAT',
     &                    ADDRESS,RSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        RPTR=DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_VARIANCE('RESULTS','UPDATE','FLOAT',
     &                        ADDRESS,RVSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        RVPTR=DYN_ELEMENT(ADDRESS)

C
C   - extract the necessary information for each
C
      DO ICOMP=1,PEAKS
C
C   - convert component number to a character string.
C
        CHCOMP=ICH_CI(ICOMP)
C
C   - find the peaks at overall maximum pixels.
C
        STRING='Finding peak '
        DUMINT=ICH_ENCODE(STRING,REAL(ICOMP),14,0,NEXT)
        STRING(NEXT:)='...'
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
C
        IF(FINDMAX)THEN
          IF(.NOT.BADPIX)THEN
            CALL MOMENTS_FINDMAX_R
     &        (DYNAMIC_MEM(BINPTR),DYNAMIC_MEM(CMPPTR),
     &         DYNAMIC_MEM(PKPTR),DYNAMIC_MEM(USPTR),
     &         DIMS(1),DIMS(2),DIMS(3),BINDIMS(1),BINDIMS(2),
     &         BINDIMS(3),THRESH,WIDTH,GAP,BIN)
          ELSE
            CALL MOMENTS_FINDMAX_RQ
     &        (DYNAMIC_MEM(BINPTR),DYNAMIC_MEM(CMPPTR),
     &         DYNAMIC_MEM(PKPTR),DYNAMIC_MEM(USPTR),
     &         DIMS(1),DIMS(2),DIMS(3),BINDIMS(1),BINDIMS(2),
     &         BINDIMS(3),THRESH,WIDTH,GAP,BIN)
          END IF
C
C   - alternatively, find the peaks in sequence from pixel 1.
C
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL MOMENTS_FINDSEQ_R
     &        (DYNAMIC_MEM(BINPTR),DYNAMIC_MEM(CMPPTR),
     &         DYNAMIC_MEM(PKPTR),DIMS(2),DIMS(3),
     &         BINDIMS(1),BINDIMS(2),BINDIMS(3),THRESH,WIDTH,GAP,BIN)
          ELSE
            CALL MOMENTS_FINDSEQ_RQ
     &        (DYNAMIC_MEM(BINPTR),DYNAMIC_MEM(CMPPTR),
     &         DYNAMIC_MEM(PKPTR),DIMS(2),DIMS(3),
     &         BINDIMS(1),BINDIMS(2),BINDIMS(3),THRESH,WIDTH,GAP,BIN)
          END IF
        END IF
C
C   - compute moments.
C
        CALL DSA_WRUSER('Computing moments...\\N')
        IF(.NOT.BADPIX)THEN
          IF (WEIGHT) THEN
            CALL MOMENTS_COMPUTE_RV
     &        (DYNAMIC_MEM(BINPTR),DYNAMIC_MEM(PKPTR),
     &         DYNAMIC_MEM(AXPTR),DYNAMIC_MEM(RPTR),
     &         DYNAMIC_MEM(RVPTR),DYNAMIC_MEM(VBINPTR),
     &         DIMS(1),DIMS(2),DIMS(3),BINDIMS(1),BINDIMS(2),BINDIMS(3),
     &         ICOMP,WIDTH,BIN,NPAR,PEAKS)
          ELSE
            CALL MOMENTS_COMPUTE_R
     &        (DYNAMIC_MEM(BINPTR),DYNAMIC_MEM(PKPTR),
     &         DYNAMIC_MEM(AXPTR),DYNAMIC_MEM(RPTR),
     &         DYNAMIC_MEM(RVPTR),
     &         DIMS(1),DIMS(2),DIMS(3),BINDIMS(1),BINDIMS(2),BINDIMS(3),
     &         ICOMP,WIDTH,BIN,NPAR,PEAKS)
          END IF
        ELSE
          IF (WEIGHT) THEN
            CALL MOMENTS_COMPUTE_RQV
     &        (DYNAMIC_MEM(BINPTR),DYNAMIC_MEM(PKPTR),
     &         DYNAMIC_MEM(AXPTR),DYNAMIC_MEM(RPTR),
     &         DYNAMIC_MEM(RVPTR),DYNAMIC_MEM(VBINPTR),
     &         DIMS(1),DIMS(2),DIMS(3),BINDIMS(1),BINDIMS(2),BINDIMS(3),
     &         ICOMP,WIDTH,BIN,NPAR,PEAKS)
          ELSE
            CALL MOMENTS_COMPUTE_RQ
     &        (DYNAMIC_MEM(BINPTR),DYNAMIC_MEM(PKPTR),
     &         DYNAMIC_MEM(AXPTR),DYNAMIC_MEM(RPTR),
     &         DYNAMIC_MEM(RVPTR),
     &         DIMS(1),DIMS(2),DIMS(3),BINDIMS(1),BINDIMS(2),BINDIMS(3),
     &         ICOMP,WIDTH,BIN,NPAR,PEAKS)
          END IF
        END IF
C
C   - unmap the RESULT arrays.
C
        CALL DSA_UNMAP(RVSLOT,STATUS)
        CALL DSA_UNMAP(RSLOT,STATUS)
C
      END DO
C
C   If a mask is being used, open the file and map the data array again. This
C   daft exercise is necessitated by the limited number of reference slots.
C   Otherwise, map four bytes of dummy workspace as before.
C
      IF(USEMASK)THEN
        CALL DSA_NAMED_INPUT('MASK',ACT_NAME,STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_USE_FLAGGED_VALUES('MASK',STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_MAP_DATA('MASK','READ','FLOAT',ADDRESS,MSKSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        MSKPTR=DYN_ELEMENT(ADDRESS)
      ELSE
        ELEM=DSA_TYPESIZE('FLOAT',STATUS)
        CALL DSA_GET_WORKSPACE(ELEM,ADDRESS,MSKSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        MSKPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C   Tidy and exit.
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END



      SUBROUTINE MOMENTS_COMPUTE_RQ
     &  (BINNED,PEAKPOS,AXIS,RESULTS,RESVARS,
     &   NX,NY,NZ,NBX,NBY,NBZ,ICOMP,WIDTH,BIN,NPAR,PEAKS)
C
      IMPLICIT NONE
C
C   Parameters.
C
      INTEGER     NX,NY,NZ,
     &            NBX,NBY,NBZ,
     &            PEAKPOS(NBY,NBZ),
     &            ICOMP,
     &            BIN,
     &            NPAR,
     &            PEAKS
      REAL        BINNED(NBX,NBY,NBZ),
     &            AXIS(NX),
     &            RESULTS(NPAR,NY,NZ,PEAKS),
     &            RESVARS(NPAR,NY,NZ,PEAKS),
     &            WIDTH
C
C   Local variables.
C
      LOGICAL     BADPIX
      LOGICAL     BADSK
      INTEGER     ENDPIX
      INTEGER     HALF
      INTEGER     I
      INTEGER     J
      INTEGER     JEN
      INTEGER     JJ
      INTEGER     JST
      INTEGER     K
      INTEGER     KEN
      INTEGER     KK
      INTEGER     KST
      REAL        KURT
      REAL        MEAN
      INTEGER     NPTS
      INTEGER     PEAKCTR
      REAL        POSN
      REAL        SDEV
      REAL        SKEW
      INTEGER     STAPIX
      REAL        TEMP1
      REAL        TEMP2
      REAL        VARI
      REAL        BINVAL
c
      include 'MAGIC_VALUES'
c
c   Initialize.
c
      badpix=.false.
      half=int(0.5*width)
      peakctr=1
c
c   For each binned spectrum -
c
      do kk=1,nbz
        do jj=1,nby
c
          jst=(jj-1)*bin+1
          jen=jst+bin-1
          kst=(kk-1)*bin+1
          ken=kst+bin-1
c
c   - check that a peak was found.
c
          if(peakpos(jj,kk).gt.magic_int)then
c
c   - initialize.
c
            npts=0
            vari=0.0
            skew=0.0
            kurt=0.0
            temp1=0.0
            temp2=0.0
            stapix=max(peakpos(jj,kk)-half,1)
            endpix=min(peakpos(jj,kk)+half,nx)
c
c   - first pass to compute the total flux and peak position.
c
            do i=stapix,endpix
              binval=binned(i,jj,kk)
              if(binval.gt.magic_float)then
                temp1=temp1+binval
                temp2=temp2+binval*axis(i)
                npts=npts+1
              end if
            end do
            mean=temp1/npts
            posn=temp2/temp1
c
c    - store the total flux and peak position.
c
            do k=kst,ken
              do j=jst,jen
                results(1,j,k,icomp)=temp1
                results(2,j,k,icomp)=posn
              end do
            end do
c
c    - second pass to compute the second, third, and fourth moments.
c
            do i=stapix,endpix
              binval=binned(i,jj,kk)
              if(binval.gt.magic_float)then
                temp1=binval-mean
                temp2=temp1*temp1
                vari=vari+temp2
                temp2=temp2*temp1
                skew=skew+temp2
                temp2=temp2*temp1
                kurt=kurt+temp2
              end if
            end do
            vari=vari/real(npts-1)
            sdev=sqrt(vari)
c
c   - skew and kurtosis are defined only when the variance is non-zero.
c
            if(vari.ne.0.0)then
              skew=skew/(real(npts)*sdev**3)
              kurt=kurt/(real(npts)*vari**2)-3.0
            else
              badsk=.true.
              skew=magic_float
              kurt=magic_float
            end if
c
c    - store the results.
c
            do k=kst,ken
              do j=jst,jen
                results(3,j,k,icomp)=vari
                results(4,j,k,icomp)=skew
                results(5,j,k,icomp)=kurt
              end do
            end do
c
c    Compute variance values
c
            do k=kst,ken
              do j=jst,jen
                resvars(1,j,k,icomp)=vari
                resvars(2,j,k,icomp)=vari/real(npts)
                resvars(3,j,k,icomp)=2.0*vari**2/real(npts)
                resvars(4,j,k,icomp)=6.0*sdev**3/real(npts)
                resvars(5,j,k,icomp)=96.0*vari**2/real(npts)
              end do
            end do
c
c    Store magic value if a peak was not present.
c
          else
            do k=kst,ken
              do j=jst,jen
                badpix=.true.
                results(1,j,k,icomp)=magic_float
                results(2,j,k,icomp)=magic_float
                results(3,j,k,icomp)=magic_float
                results(4,j,k,icomp)=magic_float
                results(5,j,k,icomp)=magic_float
              end do
            end do
          end if
c
        end do
      end do
c
c   Extrapolate data arrays to the edges of the image.
c
      if(nby*bin .lt. ny)then
        do k=1,nz
          do j=nby*bin+1,ny
            results(1,j,k,icomp)=results(1,nby*bin,k,icomp)
            resvars(1,j,k,icomp)=resvars(1,nby*bin,k,icomp)
            results(2,j,k,icomp)=results(2,nby*bin,k,icomp)
            resvars(2,j,k,icomp)=resvars(2,nby*bin,k,icomp)
            results(3,j,k,icomp)=results(3,nby*bin,k,icomp)
            resvars(3,j,k,icomp)=resvars(3,nby*bin,k,icomp)
            results(4,j,k,icomp)=results(4,nby*bin,k,icomp)
            resvars(4,j,k,icomp)=resvars(4,nby*bin,k,icomp)
            results(5,j,k,icomp)=results(5,nby*bin,k,icomp)
            resvars(5,j,k,icomp)=resvars(5,nby*bin,k,icomp)
          end do
        end do
      end if
c
      if(nbz*bin .lt. nz)then
        do k=nbz*bin+1,nz
          do j=1,ny
            results(1,j,k,icomp)=results(1,j,nbz*bin,icomp)
            resvars(1,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
            results(2,j,k,icomp)=results(2,j,nbz*bin,icomp)
            resvars(2,j,k,icomp)=resvars(2,j,nbz*bin,icomp)
            results(3,j,k,icomp)=results(3,j,nbz*bin,icomp)
            resvars(3,j,k,icomp)=resvars(3,j,nbz*bin,icomp)
            results(4,j,k,icomp)=results(4,j,nbz*bin,icomp)
            resvars(4,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
            results(5,j,k,icomp)=results(5,j,nbz*bin,icomp)
            resvars(5,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
          end do
        end do
      end if
c
      end



      SUBROUTINE MOMENTS_FINDMAX_R
     &  (BINNED,COMP,PEAKPOS,USED,NX,NY,NZ,NBX,NBY,NBZ,THRESH,WIDTH,
     &  GAP,BIN)
C
      IMPLICIT NONE
C
C   Parameters.
C
      INTEGER      NX,NY,NZ,
     &             NBX,NBY,NBZ,
     &             PEAKPOS(NBY,NBZ),
     &             BIN
      BYTE         COMP(NY,NZ),
     &             USED(NBX,NBY,NBZ)
      REAL         BINNED(NBX,NBY,NBZ),
     &             THRESH,
     &             WIDTH,
     &             GAP
C
C   Local variables.
C
      INTEGER      ENDPIX
      LOGICAL      FOUND
      INTEGER      HALF
      INTEGER      I
      INTEGER      J
      INTEGER      JEN
      INTEGER      JJ
      INTEGER      JST
      INTEGER      K
      INTEGER      KEN
      INTEGER      KK
      INTEGER      KST
      INTEGER      MAXPIX
      REAL         MAXVAL
      INTEGER      STAPIX
C
      include 'NUMERIC_RANGES'
      include 'MAGIC_VALUES'
c
      half=int(0.5*width)
c
c   For each binned spectrum -
c
      do kk=1,nbz
        do jj=1,nby
c
c - initialize.
c
          found=.false.
          maxval=min_float
c
c   - check whether a magic value has been stored, meaning there are no more
c     peaks to be found. Scan through the spectrum, avoiding any masked areas.
c
          if(peakpos(jj,kk).gt.magic_int)then
              do i=1,nbx
                if(used(i,jj,kk).eq.0)then
                    if(binned(i,jj,kk).ge.thresh)then
                      found=.true.
                      if(binned(i,jj,kk).gt.maxval)then
                        maxval=binned(i,jj,kk)
                        maxpix=i
                      end if
                    end if
                end if
              end do
           end if
c
c   - if a peak was found, store the maximum pixel number.
c
          if(found)then
            peakpos(jj,kk)=maxpix
c
c   - mask WIDTH pixels around the maximum in the usage array.
c
            stapix=max(maxpix-half,1)
            endpix=min(maxpix+half,nbx)
            do i=stapix,endpix
              used(i,jj,kk)=1
            end do
c
c   - update the component count array.
c
            jst=(jj-1)*bin+1
            jen=jst+bin-1
            kst=(kk-1)*bin+1
            ken=kst+bin-1
            do k=kst,ken
              do j=jst,jen
                comp(j,k)=comp(j,k)+1
              end do
            end do
c
c   - if a peak was not found, store the magic value.
c
          else
            peakpos(jj,kk)=magic_int
          end if
c
        end do
      end do
c
c   Extrapolate component count array to the edges of the image.
c
      if(nby*bin .lt. ny)then
        do k=1,nz
          do j=nby*bin+1,ny
            comp(j,k)=comp(nby*bin,k)
          end do
        end do
      end if
c
      if(nbz*bin .lt. nz)then
        do k=nbz*bin+1,nz
          do j=1,ny
            comp(j,k)=comp(j,nbz*bin)
          end do
        end do
      end if
c
      end





      SUBROUTINE MOMENTS_FINDSEQ_R
     &  (BINNED,COMP,PEAKPOS,NY,NZ,NBX,NBY,NBZ,THRESH,WIDTH,GAP,BIN)
C
      IMPLICIT NONE
C
C   Parameters.
C
      INTEGER      NY,NZ,
     &             NBX,NBY,NBZ,
     &             PEAKPOS(NBY,NBZ),
     &             BIN
      BYTE         COMP(NY,NZ)
      REAL         BINNED(NBX,NBY,NBZ),
     &             THRESH,
     &             WIDTH,
     &             GAP
C
C   Local variables.
C
      LOGICAL      FOUND
      INTEGER      HALF
      INTEGER      I
      INTEGER      J
      INTEGER      JEN
      INTEGER      JJ
      INTEGER      JST
      INTEGER      K
      INTEGER      KEN
      INTEGER      KK
      INTEGER      KST
      INTEGER      MAXPIX
      REAL         MAXVAL
      INTEGER      STAPIX
C
      include 'NUMERIC_RANGES'
      include 'MAGIC_VALUES'
c
      half=int(0.5*width)
c
c   For each binned spectrum -
c
      do kk=1,nbz
        do jj=1,nby
c
c   - initialize.
c
          found=.false.
          maxval=min_float
c
c   - check whether a magic value has been stored, meaning there are no more
c     peaks to be found. Also check whether the spectrum been searched to
c     within GAP of the last pixel, in which case it is not worth searching
c     the remainder.
c
          if(peakpos(jj,kk).gt.magic_int .and.
     &       peakpos(jj,kk).lt.nbx-nint(gap))then
c
c   - decide on the first pixel to be searched. If a peak has already been
c     found, advance by (GAP - 0.5*WIDTH) to reach the new start point, or if
c     GAP < WIDTH, by (0.5*WIDTH). If no peak has yet been found, start at
c     pixel 1.
c
            if(peakpos(jj,kk).gt.0)then
              if(gap.ge.width)then
                stapix=peakpos(jj,kk)+nint(gap-0.5*width)
              else
                stapix=peakpos(jj,kk)+half
              end if
            else
              stapix=1
            end if
c
c   - scan through the spectrum, starting after any previous peak. As soon as
c     a flux value above THRESH is encountered, assume that a peak has been
c     found (this requires THRESH to be carefully chosen). Compare each
c     subsequent value with the maximum value so far and store the pixel number
c     of the new maximum. If the pixel is more than (0.5*WIDTH) from the
c     maximum pixel and a new maximum has not been found, assume that the peak
c     has ended and store the maximum pixel. Likewise if the flux drops below
c     THRESH, assume that the peak has ended.
c
            do i=stapix,nbx
cq            if(binned(i,jj,kk).gt.magic_float)then
                if(binned(i,jj,kk).ge.thresh)then
                  found=.true.
                  if(binned(i,jj,kk).gt.maxval)then
                    maxval=binned(i,jj,kk)
                    maxpix=i
                  else
                    if(i.gt.maxpix+half)go to 10
                  end if
                else
                  if(found)go to 10
                end if
cq            end if
            end do
   10      continue
          end if
c
c   - if a peak was found, store the maximum pixel number.
c
          if(found)then
            peakpos(jj,kk)=maxpix
c
c   - update the component count array.
c
            jst=(jj-1)*bin+1
            jen=jst+bin-1
            kst=(kk-1)*bin+1
            ken=kst+bin-1
            do k=kst,ken
              do j=jst,jen
                comp(j,k)=comp(j,k)+1
              end do
            end do
c
c   - if a peak was not found, store the magic value.
c
          else
            peakpos(jj,kk)=magic_int
          end if
c
        end do
      end do
c
c   Extrapolate component count array to the edges of the image.
c
      if(nby*bin .lt. ny)then
        do k=1,nz
          do j=nby*bin+1,ny
            comp(j,k)=comp(nby*bin,k)
          end do
        end do
      end if
c
      if(nbz*bin .lt. nz)then
        do k=nbz*bin+1,nz
          do j=1,ny
            comp(j,k)=comp(j,nbz*bin)
          end do
        end do
      end if
c
      end





      SUBROUTINE MOMENTS_COMPUTE_R
     &  (BINNED,PEAKPOS,AXIS,RESULTS,RESVARS,
     &   NX,NY,NZ,NBX,NBY,NBZ,ICOMP,WIDTH,BIN,NPAR,PEAKS)
C
      IMPLICIT NONE
C
C   Parameters.
C
      INTEGER     NX,NY,NZ,
     &            NBX,NBY,NBZ,
     &            PEAKPOS(NBY,NBZ),
     &            ICOMP,
     &            BIN,
     &            NPAR,
     &            PEAKS
      REAL        BINNED(NBX,NBY,NBZ),
     &            AXIS(NX),
     &            RESULTS(NPAR,NY,NZ,PEAKS),
     &            RESVARS(NPAR,NY,NZ,PEAKS),
     &            WIDTH
C
C   Local variables.
C
      LOGICAL     BADPIX
      LOGICAL     BADSK
      INTEGER     ENDPIX
      INTEGER     HALF
      INTEGER     I
      INTEGER     J
      INTEGER     JEN
      INTEGER     JJ
      INTEGER     JST
      INTEGER     K
      INTEGER     KEN
      INTEGER     KK
      INTEGER     KST
      REAL        KURT
      REAL        MEAN
      INTEGER     NPTS
      INTEGER     PEAKCTR
      REAL        POSN
      REAL        SDEV
      REAL        SKEW
      INTEGER     STAPIX
      REAL        TEMP1
      REAL        TEMP2
      REAL        VARI
      REAL        BINVAL
c
      include 'MAGIC_VALUES'
c
c   Initialize.
c
      badpix=.false.
      half=int(0.5*width)
      peakctr=1
c
c   For each binned spectrum -
c
      do kk=1,nbz
        do jj=1,nby
c
          jst=(jj-1)*bin+1
          jen=jst+bin-1
          kst=(kk-1)*bin+1
          ken=kst+bin-1
c
c   - check that a peak was found.
c
          if(peakpos(jj,kk).gt.magic_int)then
c
c   - initialize.
c
            npts=0
            vari=0.0
            skew=0.0
            kurt=0.0
            temp1=0.0
            temp2=0.0
            stapix=max(peakpos(jj,kk)-half,1)
            endpix=min(peakpos(jj,kk)+half,nx)
c
c   - first pass to compute the total flux and peak position.
c
            do i=stapix,endpix
               binval=binned(i,jj,kk)
               temp1=temp1+binval
               temp2=temp2+binval*axis(i)
               npts=npts+1
            end do
            mean=temp1/npts
            posn=temp2/temp1
c
c    - store the total flux and peak position.
c
            do k=kst,ken
              do j=jst,jen
                results(1,j,k,icomp)=temp1
                results(2,j,k,icomp)=posn
              end do
            end do
c
c    - second pass to compute the second, third, and fourth moments.
c
            do i=stapix,endpix
              temp1=binned(i,jj,kk)-mean
              temp2=temp1*temp1
              vari=vari+temp2
              temp2=temp2*temp1
              skew=skew+temp2
              temp2=temp2*temp1
              kurt=kurt+temp2
            end do
            vari=vari/real(npts-1)
            sdev=sqrt(vari)
c
c   - skew and kurtosis are defined only when the variance is non-zero.
c
            if(vari.ne.0.0)then
              skew=skew/(real(npts)*sdev**3)
              kurt=kurt/(real(npts)*vari**2)-3.0
            else
              badsk=.true.
              skew=magic_float
              kurt=magic_float
            end if
c
c    - store the results.
c
            do k=kst,ken
              do j=jst,jen
                results(3,j,k,icomp)=vari
                results(4,j,k,icomp)=skew
                results(5,j,k,icomp)=kurt
              end do
            end do
c
c    Compute variance values
c
            do k=kst,ken
              do j=jst,jen
                resvars(1,j,k,icomp)=vari
                resvars(2,j,k,icomp)=vari/real(npts)
                resvars(3,j,k,icomp)=2.0*vari**2/real(npts)
                resvars(4,j,k,icomp)=6.0*sdev**3/real(npts)
                resvars(5,j,k,icomp)=96.0*vari**2/real(npts)
              end do
            end do
c
c    Store magic value if a peak was not present.
c
          else
            do k=kst,ken
              do j=jst,jen
                badpix=.true.
                results(1,j,k,icomp)=magic_float
                results(2,j,k,icomp)=magic_float
                results(3,j,k,icomp)=magic_float
                results(4,j,k,icomp)=magic_float
                results(5,j,k,icomp)=magic_float
              end do
            end do
          end if
c
        end do
      end do
c
c   Extrapolate data arrays to the edges of the image.
c
      if(nby*bin .lt. ny)then
        do k=1,nz
          do j=nby*bin+1,ny
            results(1,j,k,icomp)=results(1,nby*bin,k,icomp)
            resvars(1,j,k,icomp)=resvars(1,nby*bin,k,icomp)
            results(2,j,k,icomp)=results(2,nby*bin,k,icomp)
            resvars(2,j,k,icomp)=resvars(2,nby*bin,k,icomp)
            results(3,j,k,icomp)=results(3,nby*bin,k,icomp)
            resvars(3,j,k,icomp)=resvars(3,nby*bin,k,icomp)
            results(4,j,k,icomp)=results(4,nby*bin,k,icomp)
            resvars(4,j,k,icomp)=resvars(4,nby*bin,k,icomp)
            results(5,j,k,icomp)=results(5,nby*bin,k,icomp)
            resvars(5,j,k,icomp)=resvars(5,nby*bin,k,icomp)
          end do
        end do
      end if
c
      if(nbz*bin .lt. nz)then
        do k=nbz*bin+1,nz
          do j=1,ny
            results(1,j,k,icomp)=results(1,j,nbz*bin,icomp)
            resvars(1,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
            results(2,j,k,icomp)=results(2,j,nbz*bin,icomp)
            resvars(2,j,k,icomp)=resvars(2,j,nbz*bin,icomp)
            results(3,j,k,icomp)=results(3,j,nbz*bin,icomp)
            resvars(3,j,k,icomp)=resvars(3,j,nbz*bin,icomp)
            results(4,j,k,icomp)=results(4,j,nbz*bin,icomp)
            resvars(4,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
            results(5,j,k,icomp)=results(5,j,nbz*bin,icomp)
            resvars(5,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
          end do
        end do
      end if
c
      end

      SUBROUTINE MOMENTS_FINDMAX_RQ
     &  (BINNED,COMP,PEAKPOS,USED,NX,NY,NZ,NBX,NBY,NBZ,THRESH,WIDTH,
     &  GAP,BIN)
C
      IMPLICIT NONE
C
C   Parameters.
C
      INTEGER      NX,NY,NZ,
     &             NBX,NBY,NBZ,
     &             PEAKPOS(NBY,NBZ),
     &             BIN
      BYTE         COMP(NY,NZ),
     &             USED(NBX,NBY,NBZ)
      REAL         BINNED(NBX,NBY,NBZ),
     &             THRESH,
     &             WIDTH,
     &             GAP
C
C   Local variables.
C
      INTEGER      ENDPIX
      LOGICAL      FOUND
      INTEGER      HALF
      INTEGER      I
      INTEGER      J
      INTEGER      JEN
      INTEGER      JJ
      INTEGER      JST
      INTEGER      K
      INTEGER      KEN
      INTEGER      KK
      INTEGER      KST
      INTEGER      MAXPIX
      REAL         MAXVAL
      INTEGER      STAPIX
C
      include 'NUMERIC_RANGES'
      include 'MAGIC_VALUES'
c
      half=int(0.5*width)
c
c   For each binned spectrum -
c
      do kk=1,nbz
        do jj=1,nby
c
c   - initialize.
c
          found=.false.
          maxval=min_float
c
c   - check whether a magic value has been stored, meaning there are no more
c     peaks to be found. Scan through the spectrum, avoiding any masked areas.
c
          if(peakpos(jj,kk).gt.magic_int)then
              do i=1,nbx
                if(used(i,jj,kk).eq.0)then
                  if(binned(i,jj,kk).gt.magic_float)then
                    if(binned(i,jj,kk).ge.thresh)then
                      found=.true.
                      if(binned(i,jj,kk).gt.maxval)then
                        maxval=binned(i,jj,kk)
                        maxpix=i
                      end if
                    end if
                  end if
                end if
              end do
           end if
c
c   - if a peak was found, store the maximum pixel number.
c
          if(found)then
            peakpos(jj,kk)=maxpix
c
c   - mask WIDTH pixels around the maximum in the usage array.
c
            stapix=max(maxpix-half,1)
            endpix=min(maxpix+half,nbx)
            do i=stapix,endpix
              used(i,jj,kk)=1
            end do
c
c   - update the component count array.
c
            jst=(jj-1)*bin+1
            jen=jst+bin-1
            kst=(kk-1)*bin+1
            ken=kst+bin-1
            do k=kst,ken
              do j=jst,jen
                comp(j,k)=comp(j,k)+1
              end do
            end do
c
c   - if a peak was not found, store the magic value.
c
          else
            peakpos(jj,kk)=magic_int
          end if
c
        end do
      end do
c
c   Extrapolate component count array to the edges of the image.
c
      if(nby*bin .lt. ny)then
        do k=1,nz
          do j=nby*bin+1,ny
            comp(j,k)=comp(nby*bin,k)
          end do
        end do
      end if
c
      if(nbz*bin .lt. nz)then
        do k=nbz*bin+1,nz
          do j=1,ny
            comp(j,k)=comp(j,nbz*bin)
          end do
        end do
      end if
c
      end




      SUBROUTINE MOMENTS_FINDSEQ_RQ
     &  (BINNED,COMP,PEAKPOS,NY,NZ,NBX,NBY,NBZ,THRESH,WIDTH,GAP,BIN)
C
      IMPLICIT NONE
C
C   Parameters.
C
      INTEGER      NY,NZ,
     &             NBX,NBY,NBZ,
     &             PEAKPOS(NBY,NBZ),
     &             BIN
      BYTE         COMP(NY,NZ)
      REAL         BINNED(NBX,NBY,NBZ),
     &             THRESH,
     &             WIDTH,
     &             GAP
C
C   Local variables.
C
      LOGICAL      FOUND
      INTEGER      HALF
      INTEGER      I
      INTEGER      J
      INTEGER      JEN
      INTEGER      JJ
      INTEGER      JST
      INTEGER      K
      INTEGER      KEN
      INTEGER      KK
      INTEGER      KST
      INTEGER      MAXPIX
      REAL         MAXVAL
      INTEGER      STAPIX
C
      include 'NUMERIC_RANGES'
      include 'MAGIC_VALUES'
c
      half=int(0.5*width)
c
c   For each binned spectrum -
c
      do kk=1,nbz
        do jj=1,nby
c
c   - initialize.
c
          found=.false.
          maxval=min_float
c
c   - check whether a magic value has been stored, meaning there are no more
c     peaks to be found. Also check whether the spectrum been searched to
c     within GAP of the last pixel, in which case it is not worth searching
c     the remainder.
c
          if(peakpos(jj,kk).gt.magic_int .and.
     &       peakpos(jj,kk).lt.nbx-nint(gap))then
c
c   - decide on the first pixel to be searched. If a peak has already been
c     found, advance by (GAP - 0.5*WIDTH) to reach the new start point, or if
c     GAP < WIDTH, by (0.5*WIDTH). If no peak has yet been found, start at
c     pixel 1.
c
            if(peakpos(jj,kk).gt.0)then
              if(gap.ge.width)then
                stapix=peakpos(jj,kk)+nint(gap-0.5*width)
              else
                stapix=peakpos(jj,kk)+half
              end if
            else
              stapix=1
            end if
c
c   - scan through the spectrum, starting after any previous peak. As soon as
c     a flux value above THRESH is encountered, assume that a peak has been
c     found (this requires THRESH to be carefully chosen). Compare each
c     subsequent value with the maximum value so far and store the pixel number
c     of the new maximum. If the pixel is more than (0.5*WIDTH) from the
c     maximum pixel and a new maximum has not been found, assume that the peak
c     has ended and store the maximum pixel. Likewise if the flux drops below
c     THRESH, assume that the peak has ended.
c
            do i=stapix,nbx
                if(binned(i,jj,kk).ge.thresh)then
                  found=.true.
                  if(binned(i,jj,kk).gt.maxval)then
                    maxval=binned(i,jj,kk)
                    maxpix=i
                  else
                    if(i.gt.maxpix+half)go to 10
                  end if
                else
                  if(found)go to 10
                end if
            end do
   10      continue
          end if
c
c   - if a peak was found, store the maximum pixel number.
c
          if(found)then
            peakpos(jj,kk)=maxpix
c
c   - update the component count array.
c
            jst=(jj-1)*bin+1
            jen=jst+bin-1
            kst=(kk-1)*bin+1
            ken=kst+bin-1
            do k=kst,ken
              do j=jst,jen
                comp(j,k)=comp(j,k)+1
              end do
            end do
c
c   - if a peak was not found, store the magic value.
c
          else
            peakpos(jj,kk)=magic_int
          end if
c
        end do
      end do
c
c   Extrapolate component count array to the edges of the image.
c
      if(nby*bin .lt. ny)then
        do k=1,nz
          do j=nby*bin+1,ny
            comp(j,k)=comp(nby*bin,k)
          end do
        end do
      end if
c
      if(nbz*bin .lt. nz)then
        do k=nbz*bin+1,nz
          do j=1,ny
            comp(j,k)=comp(j,nbz*bin)
          end do
        end do
      end if
c
      end


      SUBROUTINE MOMENTS_COMPUTE_RV
     &  (BINNED,PEAKPOS,AXIS,RESULTS,RESVARS,VBIN,
     &   NX,NY,NZ,NBX,NBY,NBZ,ICOMP,WIDTH,BIN,NPAR,PEAKS)
C
      IMPLICIT NONE
C
C   Parameters.
C
      INTEGER     NX,NY,NZ,
     &            NBX,NBY,NBZ,
     &            PEAKPOS(NBY,NBZ),
     &            ICOMP,
     &            BIN,
     &            NPAR,
     &            PEAKS
      REAL        BINNED(NBX,NBY,NBZ),
     &            VBIN(NBX,NBY,NBZ),
     &            AXIS(NX),
     &            RESULTS(NPAR,NY,NZ,PEAKS),
     &            RESVARS(NPAR,NY,NZ,PEAKS),
     &            WIDTH
C
C   Local variables.
C
      LOGICAL     BADPIX
      LOGICAL     BADSK
      INTEGER     ENDPIX
      INTEGER     HALF
      INTEGER     I
      INTEGER     J
      INTEGER     JEN
      INTEGER     JJ
      INTEGER     JST
      INTEGER     K
      INTEGER     KEN
      INTEGER     KK
      INTEGER     KST
      REAL        KURT
      REAL        MEAN
      INTEGER     NPTS
      INTEGER     PEAKCTR
      REAL        POSN
      REAL        SDEV
      REAL        SKEW
      INTEGER     STAPIX
      REAL        TEMP1
      REAL        TEMP2
      REAL        VARI
      REAL        WT,BINVAL,WTSUM
c
      include 'MAGIC_VALUES'
c
c   Initialize.
c
      badpix=.false.
      half=int(0.5*width)
      peakctr=1
c
c   For each binned spectrum -
c
      do kk=1,nbz
        do jj=1,nby
c
          jst=(jj-1)*bin+1
          jen=jst+bin-1
          kst=(kk-1)*bin+1
          ken=kst+bin-1
c
c   - check that a peak was found.
c
          if(peakpos(jj,kk).gt.magic_int)then
c
c   - initialize.
c
            npts=0
            vari=0.0
            skew=0.0
            kurt=0.0
            temp1=0.0
            temp2=0.0
            wtsum=0.0
            stapix=max(peakpos(jj,kk)-half,1)
            endpix=min(peakpos(jj,kk)+half,nx)
c
c   - first pass to compute the total flux and peak position.
c
            do i=stapix,endpix
               binval=binned(i,jj,kk)
               wt=1.0/vbin(i,jj,kk)
               temp1=temp1+binval*wt
               temp2=temp2+binval*axis(i)*wt
               wtsum=wtsum+wt
            end do
            mean=temp1/wtsum
            posn=temp2/temp1
c
c    - store the total flux and peak position.
c
            do k=kst,ken
              do j=jst,jen
                results(1,j,k,icomp)=temp1
                results(2,j,k,icomp)=posn
              end do
            end do
c
c    - second pass to compute the second, third, and fourth moments.
c
            do i=stapix,endpix
              temp1=(binned(i,jj,kk))-mean
              temp2=temp1*temp1
              wt=1.0/vbin(i,jj,kk)
              vari=vari+temp2*wt
              temp2=temp2*temp1
              skew=skew+temp2*wt
              temp2=temp2*temp1
              kurt=kurt+temp2*wt
            end do
            vari=vari/wtsum
            sdev=sqrt(vari)
c
c   - skew and kurtosis are defined only when the variance is non-zero.
c
            if(vari.ne.0.0)then
              skew=skew/(wtsum*sdev**3)
              kurt=kurt/(wtsum*vari**2)
            else
              badsk=.true.
              skew=magic_float
              kurt=magic_float
            end if
c
c    - store the results.
c
            do k=kst,ken
              do j=jst,jen
                results(3,j,k,icomp)=vari
                results(4,j,k,icomp)=skew
                results(5,j,k,icomp)=kurt
              end do
            end do
c
c    Compute variance values
c
            do k=kst,ken
              do j=jst,jen
                resvars(1,j,k,icomp)=vari
                resvars(2,j,k,icomp)=vari/real(npts)
                resvars(3,j,k,icomp)=2.0*vari**2/real(npts)
                resvars(4,j,k,icomp)=6.0*sdev**3/real(npts)
                resvars(5,j,k,icomp)=96.0*vari**2/real(npts)
              end do
            end do
c
c    Store magic value if a peak was not present.
c
          else
            do k=kst,ken
              do j=jst,jen
                badpix=.true.
                results(1,j,k,icomp)=magic_float
                results(2,j,k,icomp)=magic_float
                results(3,j,k,icomp)=magic_float
                results(4,j,k,icomp)=magic_float
                results(5,j,k,icomp)=magic_float
              end do
            end do
          end if
c
        end do
      end do
c
c   Extrapolate data arrays to the edges of the image.
c
      if(nby*bin .lt. ny)then
        do k=1,nz
          do j=nby*bin+1,ny
            results(1,j,k,icomp)=results(1,nby*bin,k,icomp)
            resvars(1,j,k,icomp)=resvars(1,nby*bin,k,icomp)
            results(2,j,k,icomp)=results(2,nby*bin,k,icomp)
            resvars(2,j,k,icomp)=resvars(2,nby*bin,k,icomp)
            results(3,j,k,icomp)=results(3,nby*bin,k,icomp)
            resvars(3,j,k,icomp)=resvars(3,nby*bin,k,icomp)
            results(4,j,k,icomp)=results(4,nby*bin,k,icomp)
            resvars(4,j,k,icomp)=resvars(4,nby*bin,k,icomp)
            results(5,j,k,icomp)=results(5,nby*bin,k,icomp)
            resvars(5,j,k,icomp)=resvars(5,nby*bin,k,icomp)
          end do
        end do
      end if
c
      if(nbz*bin .lt. nz)then
        do k=nbz*bin+1,nz
          do j=1,ny
            results(1,j,k,icomp)=results(1,j,nbz*bin,icomp)
            resvars(1,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
            results(2,j,k,icomp)=results(2,j,nbz*bin,icomp)
            resvars(2,j,k,icomp)=resvars(2,j,nbz*bin,icomp)
            results(3,j,k,icomp)=results(3,j,nbz*bin,icomp)
            resvars(3,j,k,icomp)=resvars(3,j,nbz*bin,icomp)
            results(4,j,k,icomp)=results(4,j,nbz*bin,icomp)
            resvars(4,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
            results(5,j,k,icomp)=results(5,j,nbz*bin,icomp)
            resvars(5,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
          end do
        end do
      end if
c
      end


      SUBROUTINE MOMENTS_COMPUTE_RQV
     &  (BINNED,PEAKPOS,AXIS,RESULTS,RESVARS,VBIN,
     &   NX,NY,NZ,NBX,NBY,NBZ,ICOMP,WIDTH,BIN,NPAR,PEAKS)
C
      IMPLICIT NONE
C
C   Parameters.
C
      INTEGER     NX,NY,NZ,
     &            NBX,NBY,NBZ,
     &            PEAKPOS(NBY,NBZ),
     &            ICOMP,
     &            BIN,
     &            NPAR,
     &            PEAKS
      REAL        BINNED(NBX,NBY,NBZ),
     &            VBIN(NBX,NBY,NBZ),
     &            AXIS(NX),
     &            RESULTS(NPAR,NY,NZ,PEAKS),
     &            RESVARS(NPAR,NY,NZ,PEAKS),
     &            WIDTH
C
C   Local variables.
C
      LOGICAL     BADPIX
      LOGICAL     BADSK
      INTEGER     ENDPIX
      INTEGER     HALF
      INTEGER     I
      INTEGER     J
      INTEGER     JEN
      INTEGER     JJ
      INTEGER     JST
      INTEGER     K
      INTEGER     KEN
      INTEGER     KK
      INTEGER     KST
      REAL        KURT
      REAL        MEAN
      INTEGER     NPTS
      INTEGER     PEAKCTR
      REAL        POSN
      REAL        SDEV
      REAL        SKEW
      INTEGER     STAPIX
      REAL        TEMP1
      REAL        TEMP2
      REAL        VARI
      REAL        WT,BINVAL,WTSUM
c
      include 'MAGIC_VALUES'
c
c   Initialize.
c
      badpix=.false.
      half=int(0.5*width)
      peakctr=1
c
c   For each binned spectrum -
c
      do kk=1,nbz
        do jj=1,nby
c
          jst=(jj-1)*bin+1
          jen=jst+bin-1
          kst=(kk-1)*bin+1
          ken=kst+bin-1
c
c   - check that a peak was found.
c
          if(peakpos(jj,kk).gt.magic_int)then
c
c   - initialize.
c
            npts=0
            vari=0.0
            skew=0.0
            kurt=0.0
            temp1=0.0
            temp2=0.0
            wtsum=0.0
            stapix=max(peakpos(jj,kk)-half,1)
            endpix=min(peakpos(jj,kk)+half,nx)
c
c   - first pass to compute the total flux and peak position.
c
            do i=stapix,endpix
              if(binned(i,jj,kk).gt.magic_float)then
                binval=binned(i,jj,kk)
                wt=1.0/vbin(i,jj,kk)
                temp1=temp1+binval*wt
                temp2=temp2+binval*axis(i)*wt
                wtsum=wtsum+wt
              end if
            end do
            mean=temp1/wtsum
            posn=temp2/temp1
c
c    - store the total flux and peak position.
c
            do k=kst,ken
              do j=jst,jen
                results(1,j,k,icomp)=temp1
                results(2,j,k,icomp)=posn
              end do
            end do
c
c    - second pass to compute the second, third, and fourth moments.
c
            do i=stapix,endpix
              if(binned(i,jj,kk).gt.magic_float)then
                temp1=binned(i,jj,kk)-mean
                temp2=temp1*temp1
                wt=1.0/vbin(i,jj,kk)
                vari=vari+temp2*wt
                temp2=temp2*temp1
                skew=skew+temp2*wt
                temp2=temp2*temp1
                kurt=kurt+temp2*wt
              end if
            end do
            vari=vari/wtsum
            sdev=sqrt(vari)
c
c   - skew and kurtosis are defined only when the variance is non-zero.
c
            if(vari.ne.0.0)then
              skew=skew/(wtsum*sdev**3)
              kurt=kurt/(wtsum*vari**2)
            else
              badsk=.true.
              skew=magic_float
              kurt=magic_float
            end if
c
c    - store the results.
c
            do k=kst,ken
              do j=jst,jen
                results(3,j,k,icomp)=vari
                results(4,j,k,icomp)=skew
                results(5,j,k,icomp)=kurt
              end do
            end do
c
c    Compute variance values
c
            do k=kst,ken
              do j=jst,jen
                resvars(1,j,k,icomp)=vari
                resvars(2,j,k,icomp)=vari/real(npts)
                resvars(3,j,k,icomp)=2.0*vari**2/real(npts)
                resvars(4,j,k,icomp)=6.0*sdev**3/real(npts)
                resvars(5,j,k,icomp)=96.0*vari**2/real(npts)
              end do
            end do
c
c    Store magic value if a peak was not present.
c
          else
            do k=kst,ken
              do j=jst,jen
                badpix=.true.
                results(1,j,k,icomp)=magic_float
                results(2,j,k,icomp)=magic_float
                results(3,j,k,icomp)=magic_float
                results(4,j,k,icomp)=magic_float
                results(5,j,k,icomp)=magic_float
              end do
            end do
          end if
c
        end do
      end do
c
c   Extrapolate data arrays to the edges of the image.
c
      if(nby*bin .lt. ny)then
        do k=1,nz
          do j=nby*bin+1,ny
            results(1,j,k,icomp)=results(1,nby*bin,k,icomp)
            resvars(1,j,k,icomp)=resvars(1,nby*bin,k,icomp)
            results(2,j,k,icomp)=results(2,nby*bin,k,icomp)
            resvars(2,j,k,icomp)=resvars(2,nby*bin,k,icomp)
            results(3,j,k,icomp)=results(3,nby*bin,k,icomp)
            resvars(3,j,k,icomp)=resvars(3,nby*bin,k,icomp)
            results(4,j,k,icomp)=results(4,nby*bin,k,icomp)
            resvars(4,j,k,icomp)=resvars(4,nby*bin,k,icomp)
            results(5,j,k,icomp)=results(5,nby*bin,k,icomp)
            resvars(5,j,k,icomp)=resvars(5,nby*bin,k,icomp)
          end do
        end do
      end if
c
      if(nbz*bin .lt. nz)then
        do k=nbz*bin+1,nz
          do j=1,ny
            results(1,j,k,icomp)=results(1,j,nbz*bin,icomp)
            resvars(1,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
            results(2,j,k,icomp)=results(2,j,nbz*bin,icomp)
            resvars(2,j,k,icomp)=resvars(2,j,nbz*bin,icomp)
            results(3,j,k,icomp)=results(3,j,nbz*bin,icomp)
            resvars(3,j,k,icomp)=resvars(3,j,nbz*bin,icomp)
            results(4,j,k,icomp)=results(4,j,nbz*bin,icomp)
            resvars(4,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
            results(5,j,k,icomp)=results(5,j,nbz*bin,icomp)
            resvars(5,j,k,icomp)=resvars(1,j,nbz*bin,icomp)
          end do
        end do
      end if
c
      end

