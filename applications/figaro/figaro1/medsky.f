C+
      SUBROUTINE MEDSKY
C
C     M E D S K Y
C
C     MEDSKY is a program for constructing sky flats for direct imaging,
C     using an algorithm due to Schneider, Schmidt and Gunn.
C
C     A list of images is read in from file,and the medians of
C     equivalent pixels in all the images are found, eg. for N images
C     (of any size), the first pixel ( the top-left one) in each image
C     is fetched : the median of these N pixels is then found, the
C     result becoming the first pixel in the output image. This process
C     is repeated for all the pixels in the input images to construct a
C     complete output image. In practice, more than one pixel is
C     analysed per pass through the image file list, the number being
C     determined by the memory available to the program. MEDSKY attempts
C     to minimise the page faulting which would occur by accessing too
C     much virtual memory at once by processing fewer pixels per pass,
C     on the assumption that the extra overheads in doing more passes
C     will be more than offset by fewer page faults. This approach seems
C     to have been justified, but performance is very sensitive to
C     changes in the proportions of memory allocated, and it is possible
C     that improvements could be made.
C
C     Command parameters -
C
C     FILES  The name of a .DAT file containing a list of names
C            of images. All these images must have dimensions
C            equal to those of the first image in the list, FIRST.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for FIRST.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C     IMGLOG Only used if the LOG keyword is specified.  This supplies
C            the name of the 'log' image created.  This will contain a
C            single integer data array.
C
C     Important
C     ~~~~~~~~~
C     MEDSKY does NOT check that the output image name is not a member
C     of the image file list.
C
C     Command keywords  -
C
C     SCALED If specified, MEDSKY attempts to compensate for differences
C            in data scale between the different images.  It does this
C            by conceptually scaling all images so that they have the
C            same median value as the first image.
C
C     LOG    If specified, MEDSKY creates a 'log' image.  This is an
C            image with the same dimensions as the output image, with
C            each pixel a number from 1 to N (the number of files)
C            showing the image number which for each output pixel had
C            the data value closest to the median value calculated.
C            This can be used to see if any image dominates the others,
C            or if there is any trend across the image. This is a rather
C            specialised option, and is not expected to be used much.
C
C     User variables used - None
C                                      DJA / AAO. 16th Aug 1987
C     Modified:
C
C     21st Dec 1987  Bug fix.  Was performing median calculation on too
C                    many pixels on final pass.
C     2nd  Feb 1991  SCALED and LOG keywords and IMGLOG parameter added,
C                    together with supporting code. KS/AAO.
C     7th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TAB removed.
C                    Lowercase file name files.dat, don't fold file name
C                    got from paramter system. Lowercase file name
C                    medsky.log. Call FIG_SETERR rather than SETERR.
C                    Avoid calling GEN_???PHYSMEM by assuming 1 Mbyte
C                    maximum and 0.5 Mbyte current.
C     18th Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     27th Jan 1997  MJCL / Starlink, UCL.  I have, somewhat
C                    arbitrarily, modified the estimate of physical
C                    memory.  The old value of 1 Mbyte maximum is too low
C                    for efficient use of contemporary machines.  The
C                    value is now set at 16384 Kbytes in the parameter
C                    PHYS_AVAIL.
C     21st May 1997  MJCL / Starlink, UCL.  Initialise FAULT to .FALSE.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
CC
C     Functions:
C
      LOGICAL     PAR_ABORT
      INTEGER     ICH_LEN,ICH_CLEAN
      INTEGER     DSA_TYPESIZE
      REAL        GEN_QFMED
      CHARACTER   ICH_CF*15, ICH_CI*4
C
C     Constants
C
      INTEGER EOF
      PARAMETER (EOF=(-1))
C
C     Maximum number of images that can be handled if SCALED OR LOG
C     is specified (if neither are used, any number of files can be
C     accommodated).
C
      INTEGER MAX_FILES
      PARAMETER (MAX_FILES=500)
C
C     Estimate of memory available.
C
      INTEGER PHYS_AVAIL
      PARAMETER (PHYS_AVAIL=16384)
C
C     Local variables
C
      INTEGER   BPTR             ! Pointer to current pixel in input
                                 ! data
      INTEGER   CURBATPIX        ! Current size of batch of pixels
      INTEGER   DIMS(10)         ! Sizes of dimensions of data
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number of data array
      LOGICAL   FAULT            ! Indicates error while reading the
                                 ! input file
      LOGICAL   FIRST            ! Current image is the first one?
      LOGICAL   FIRSTPASS        ! 1st batch of pixels is being done?
      CHARACTER FILE*80          ! A particular line from the input file
      CHARACTER FILENAME*132     ! The filename of the input file
      LOGICAL   FOPEN            ! Input file was opened correctly?
      INTEGER   IFILE            ! The current file number
      INTEGER   IGNORE           ! Used to pass ignorable status
      INTEGER   INPUT            ! The logical unit number of the input
                                 ! file
      INTEGER   INVOKE           ! Used to invoke functions
      LOGICAL   ISNEW            ! Is address new to CNF?
      LOGICAL   ISNEWL           ! Is address for logs new to CNF?
      INTEGER   LOGLU            ! Logical unit for Scaling log file
      LOGICAL   LOGNS            ! Value of the LOG parameter
      INTEGER   LOGPTR           ! Dynamic-memory pointer for log image
                                 ! data
      INTEGER   LPIX             ! Number of the last pixel in the batch
      INTEGER   LSLOT            ! Map slot number log image array
      INTEGER   MAXBATPIX        ! Maximum number of pixels per pass
                                 ! through file
      REAL      MEDIANS(MAX_FILES) ! Overall median values for the
                                 ! different images
      LOGICAL   MOREFILES        ! More input images?
      LOGICAL   MOREPIX          ! More pixels to do?
      CHARACTER NAME *32         ! Full name of log file - ignored
      INTEGER   NDIM             ! Number of dimensions in data
      INTEGER   NELM             ! Total number of elements in data
      INTEGER   NFB              ! The number of bytes in a 'FLOAT'
      INTEGER   NFILE            ! Number of images in the input file
      INTEGER   NPIX             ! Number of pixels in the current batch
      INTEGER   PHYBYTES         ! Total amount of physical memory
                                 ! available
      LOGICAL   PISNL            ! Previous CNF pointer to log data new?
      LOGICAL   PISNEW           ! Previous CNF pointer to data new?
      INTEGER   PIX              ! The current pixel number
      INTEGER   OPTR             ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER   OSLOT            ! Map slot number output data array
      REAL      SCALE            ! Scale factor to apply to scaled data
      LOGICAL   SCALED           ! Value of SCALED parameter
      INTEGER   STATUS           ! Running status for DSA_ routines
      CHARACTER STRING*80        ! Used to format lines of log file
      INTEGER   TPTR             ! Temporary dynamic mem pointer
      INTEGER   WBYTES           ! Amount of workspace required per
                                 ! batch
      INTEGER   WMEDPTR          ! Dynamic-memory pointer to image-
                                 ! median work array
      REAL      WORK(MAX_FILES)  ! Work array used only if LOG is
                                 ! specified
      INTEGER   WPTR             ! Dynamic-memory pointer to workspace
      INTEGER   WSLOT            ! Map slot number of workspace
C
C     Initial values
C
      FIRST=.TRUE.
      FIRSTPASS=.TRUE.
      FOPEN=.FALSE.
      MOREFILES=.TRUE.
      MOREPIX=.TRUE.
      FAULT=.FALSE.
      STATUS=0
      NFB=DSA_TYPESIZE('FLOAT',STATUS)
      NFILE=0
      PIX=1
C
C     Initialisation of DSA_ routines
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Find the name of the input file and open it
C
      CALL DSA_GET_LU (INPUT,STATUS)
      CALL PAR_RDCHAR('FILES','files.dat',FILENAME)
      IF (PAR_ABORT()) GO TO 500
      INVOKE=ICH_CLEAN(FILENAME)
      OPEN(UNIT=INPUT, FILE=FILENAME, STATUS='OLD', IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error opening '//FILENAME(:ICH_LEN
     :                      (FILENAME))//' for input',IGNORE)
         GOTO 500
      ELSE
         FOPEN=.TRUE.
      END IF
C
C     Check phase. Count the number of image filenames in the input file.
C     All blank lines, and lines beginning with an '*' are ignored.
C     The user can thus insert comments into the list of images.
C
      DO WHILE(MOREFILES)
         READ (INPUT,'(A)',IOSTAT=STATUS) FILE
         IF (STATUS.EQ.EOF) THEN
            MOREFILES=.FALSE.
            STATUS=0
         ELSE IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('I/O error reading from data file',IGNORE)
            MOREFILES=.FALSE.
            FAULT=.TRUE.
         ELSE IF ((FILE(:1).NE.'*').AND.(FILE.GT.' ')) THEN
            IF (FIRST) THEN
               FIRST=.FALSE.
               CALL DSA_NAMED_INPUT('FIRST',FILE,STATUS)
               CALL DSA_DATA_SIZE('FIRST',10,NDIM,DIMS,NELM,STATUS)
            END IF
            NFILE=NFILE+1
         END IF
      END DO
      REWIND (UNIT=INPUT,IOSTAT=STATUS)
      IF ((FAULT).OR.(STATUS.NE.0)) GOTO 500
C
C     See if scaling is wanted.  If so, we open a file that will log the
C     files, their median values, and the scale factors used.  Moreover,
C     to determine their medians, we will require a work array into which
C     we can copy a whole image (since the median routine shuffles
C     its input data).
C
      CALL PAR_RDKEY('SCALED',.FALSE.,SCALED)
      IF (PAR_ABORT()) GO TO 500
      IF (SCALED) THEN
         IF (NFILE.GT.MAX_FILES) THEN
            CALL PAR_WRUSER('Too many files for internal scale tables',
     :                                                         IGNORE)
            FAULT=.TRUE.
            GO TO 500
         END IF
         CALL DSA_OPEN_TEXT_FILE ('medsky.log',' ','NEW',.TRUE.,LOGLU,
     :                                                   NAME,STATUS)
         IF (STATUS.NE.0) GO TO 500
         STRING='No. File'
         STRING(49:)='Median'
         STRING(64:)='Scale'
         WRITE (LOGLU,'(A)',IOSTAT=IGNORE) STRING
         STRING='--- ----------------------'
         STRING(49:)='-------------'
         STRING(64:)='-------------'
         WRITE (LOGLU,'(A)',IOSTAT=IGNORE) STRING
         CALL DSA_GET_WORK_ARRAY (NELM,'FLOAT',WMEDPTR,WSLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
      END IF
C
C     Now work out the maximum number of pixels to be processed per pass
C     through the input file.
C     Also make sure that MAXBATPIX is <= NELM, there's no point in asking
C     for space that isn't needed.
C
      PHYBYTES=1024*PHYS_AVAIL
      MAXBATPIX=MIN(NINT((0.5*PHYBYTES)/(FLOAT((2+NFILE)*NFB))),NELM)
      WBYTES=MAXBATPIX*NFILE*NFB
      CALL DSA_GET_WORKSPACE(WBYTES,WPTR,WSLOT,STATUS)
C
C     Get the name of the OUTPUT array, and map it in
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','FIRST',0,0,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     See if the rather unusual LOG keyword has been speciifed.  If so,
C     create the output log image and map it.
C
      CALL PAR_RDKEY('LOG',.FALSE.,LOGNS)
      IF (PAR_ABORT()) GO TO 500
      IF (LOGNS) THEN
         IF (NFILE.GT.MAX_FILES) THEN
            CALL PAR_WRUSER(
     :            'Too many files for internal log workspace',IGNORE)
            FAULT=.TRUE.
            GO TO 500
         END IF
         CALL DSA_OUTPUT ('IMGLOG','IMGLOG',' ',0,0,STATUS)
         CALL DSA_SIMPLE_OUTPUT ('IMGLOG','DATA','SHORT',NDIM,DIMS,
     :                                                         STATUS)
         CALL DSA_MAP_DATA ('IMGLOG','WRITE','INT',LOGPTR,LSLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
      END IF
C
C     Pass through file repeatedly until all the pixels have been
C     processed
C
      DO WHILE (MOREPIX)
C
C        For each image in the list file, map its data and transfer the
C        section containing the appropriate pixels for this pass into
C        the workspace array. The 'geometry' of the process is shown
C        below, for NFILE images (they are always treated as
C        one-dimensional) and a batch of pixels NPIX long:
C
C                                STAGE 1          STAGE 2
C
C        1st input image (NELM,1)      Work (N,NPIX)     Output (NELM,1)
C
C         <----- NELM ------->           <-NFILE->
C        +--------------------+      ^  +-------+
C        |    |segment_1|     |      |  | s   s | median_1 -+
C        +--------------------|      |  | e   e |           |
C            <--NPIX-->              N  | g   g |           |
C                ...                 P  | m . m |       +---v----------+
C                                =>  I  | e . e | ...   |  |medians|   |
C        N th input image            X  | n . n |       +---------^----+
C                                    |  | t   t |                 |
C        +--------------------+      |  |       |                 |
C        |    |segment_N|     |      |  | 1   N | median_N -------+
C        +--------------------+      v  +-------+
C
C        STAGE 1 - All the pixels in the current segment are transferred
C                  to the work array for each image.
C        STAGE 2 - The median of the values in each row of the work
C                  array is found, and repeated for every row.
C
         MOREFILES=.TRUE.
         IFILE=1
         PISNEW = .FALSE.
         DO WHILE(MOREFILES)
            READ (INPUT,'(A)',IOSTAT=STATUS) FILE
            IF ((FILE(:1).NE.'*').AND.(FILE.GT.' ').AND.
     :          (STATUS.EQ.0)) THEN
               CALL DSA_NAMED_INPUT('NEXT',FILE,STATUS)
               CALL DSA_MAP_DATA('NEXT','READ','FLOAT',DPTR,DSLOT,
     :                           STATUS)
C
C              If this the first file in the list, work out how much
C              memory is currently available. This is done here as we
C              have all the data mapped in and want to know what the
C              optimum number of pixels is to minimise page faulting.
C              This is very sensitive to the factor of PHYBYTES,
C              currently 0.75. Too high and page faulting rises steeply,
C              too low and maximum use of memory isn't being made.
C              Both will result in a longer execution time.
C
               IF (IFILE.EQ.1) THEN
                  PHYBYTES=512*PHYS_AVAIL
                  CURBATPIX=MIN(NINT((0.75*PHYBYTES)/(FLOAT((2+NFILE)*
     :              NFB))),MAXBATPIX)
                  LPIX=MIN(NELM,PIX+CURBATPIX-1)
                  NPIX=LPIX-PIX+1
                  IF (LPIX.EQ.NELM) MOREPIX=.FALSE.
               ELSE IF (FIRSTPASS) THEN
C
C                 It wasn't the first file - if this is the first pass
C                 through the list then check that the size of the
C                 current file is the same as the first in the list.
C
                  CALL DSA_MATCH_SIZES('FIRST','NEXT',STATUS)
               END IF
               IF (STATUS.NE.0) GOTO 500
C
C              Preliminary stage - If data is to be scaled, on the first
C              pass, determine the median of the image data (copying
C              it into the median workspace array first)
C
               IF (SCALED.AND.FIRSTPASS) THEN
                  CALL GEN_MOVE (NELM*NFB,%VAL(CNF_PVAL(DPTR)),
     :                           %VAL(CNF_PVAL(WMEDPTR)))
                  MEDIANS(IFILE)=GEN_QFMED(%VAL(CNF_PVAL(WMEDPTR)),NELM)

                  CALL DSA_GET_ACTUAL_NAME('NEXT',FILE,STATUS)
                  STRING=ICH_CI(IFILE)//FILE
                  STRING(49:)=ICH_CF(MEDIANS(IFILE))
                  STRING(64:)=ICH_CF(MEDIANS(1)/MEDIANS(IFILE))
                  WRITE (LOGLU,'(A)',IOSTAT=IGNORE) STRING
               END IF
C
C              STAGE 1 - Transfer data from (BPTR) to the (I)th column of the
C                        NFILE x MAXBATPIX array starting at (WPTR).
C
               IF (SCALED) THEN
                  SCALE=MEDIANS(1)/MEDIANS(IFILE)
               ELSE
                  SCALE=1.0
               END IF

               CALL DYN_INCAD(DPTR,'FLOAT',PIX-1,BPTR,ISNEW,STATUS)
               CALL FIG_MOVE_ROWTOCOL(%VAL(CNF_PVAL(BPTR)),NPIX,SCALE,
     :                                %VAL(CNF_PVAL(WPTR)),NFILE,
     :                                CURBATPIX,IFILE)
               IF (ISNEW) CALL CNF_UNREGP(BPTR)
C
C              Unmap current image
C
               CALL DSA_UNMAP(DSLOT,STATUS)
               CALL DSA_CLOSE_STRUCTURE('NEXT',STATUS)
               IFILE=IFILE+1
            ELSE IF (STATUS.EQ.EOF) THEN
               MOREFILES=.FALSE.
               STATUS=0
            ELSE IF (STATUS.NE.0) THEN
               CALL PAR_WRUSER('I/O error reading from data file',
     :                                                       IGNORE)
               FAULT=.TRUE.
               MOREFILES=.FALSE.
            END IF
         END DO
         FIRSTPASS=.FALSE.
         REWIND (UNIT=INPUT,IOSTAT=STATUS)
         IF (STATUS.NE.0) GOTO 500
C
C        STAGE 2 - We've now got all the data in the workspace array. Operate on
C                  the data, putting the results into OUTPUT.
C
         CALL FIG_MEDX2D(%VAL(CNF_PVAL(WPTR)),NFILE,NPIX,LOGNS,WORK,
     :                        %VAL(CNF_PVAL(OPTR)),
     :                   %VAL(CNF_PVAL(LOGPTR)))

         CALL DYN_INCAD(OPTR,'FLOAT',CURBATPIX,TPTR,ISNEW,STATUS)
         IF (PISNEW) CALL CNF_UNREGP(OPTR)
         OPTR = TPTR
         PISNEW = ISNEW

         IF (LOGNS) THEN
            CALL DYN_INCAD(LOGPTR,'INT',CURBATPIX,TPTR,ISNEWL,STATUS)
            IF (PISNL) CALL CNF_UNREGP(LOGPTR)
            LOGPTR = TPTR
            PISNL = ISNEWL
         END IF
         PIX=LPIX+1
      END DO
C
C     Tidy up
C
  500 CONTINUE
C
      IF (FOPEN) CLOSE(UNIT=INPUT,IOSTAT=STATUS)
C
C     Closedown DSA routines
C
      CALL DSA_CLOSE(STATUS)
C
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_MEDX2D(ARRAY,NX,NY,LOGNS,WORK,DEST,NDATA)
C
C     F I G _ M E D X 2 D
C
C     Calculate the median of all the values in each row of ARRAY, putting the
C     result for the Ith row into the Ith column of the 1D array DEST.  If
C     LOGNS is true, the corresponding elements of the array NDATA are set
C     to the element numbers with the values closest to the calculated
C     medians.
C
C     Parameters -   (">" input, "W" work "<" output)
C
C     ARRAY  (>) Real array (NX,NY) - The input data
C     NX     (>) Integer - X dimension of input array
C     NY     (>) Integer - Y dimension of input array,and length of output array
C     LOGNS  (>) Logical - True if elements of NDATA are to be set
C     WORK   (W) Real array (NX) - Workspace used only if LOGNS is true
C     DEST   (<) Real array (NY) - The array of medians
C     NDATA  (<) Integer array (NY) - The array of closest element numbers
C
C                                                  DJA / AAO 25th Aug 1987
C     Modified:
C
C     2nd Feb 1991.  LOGNS,WORK and NDATA and supporting code added. KS/AAO.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL  LOGNS
      INTEGER  NX,NY,NDATA(NY)
      REAL     ARRAY(NX,NY),WORK(NX),DEST(NY)
C
C     Functions
C
      REAL     GEN_QFMED
C
C     Local variables
C
      INTEGER  I, IX, NEAREST
      REAL DIFF, DIFFMIN, MEDVAL
C
      DO I=1,NY
         IF (LOGNS) THEN
            DO IX=1,NX
               WORK(IX)=ARRAY(IX,I)
            END DO
         END IF
         MEDVAL=GEN_QFMED(ARRAY(1,I),NX)
         DEST(I)=MEDVAL
         IF (LOGNS) THEN
            DIFFMIN=ABS(WORK(1)-MEDVAL)
            NEAREST=1
            DO IX=2,NX
               DIFF=ABS(WORK(IX)-MEDVAL)
               IF (DIFF.LT.DIFFMIN) THEN
                  DIFFMIN=DIFF
                  NEAREST=IX
               END IF
            END DO
            NDATA(I)=NEAREST
         END IF
      END DO
      END

C+
      SUBROUTINE FIG_MOVE_ROWTOCOL(LINE,N,SCALE,DEST,NX,NY,X)
C
C     F I G _ M O V E _ R O W T O C O L
C
C     MEDSKY utility routine.  Moves a one dimensional array of length N into
C     the first N rows of the (I)th column of the 2D array DEST(NX,NY).
C
C     (>) LINE      (Real array LINE(N)) The image data to be moved
C     (>) N         (Integer) The size of the array LINE
C     (>) SCALE     (Real) A scaling factor by which to multiply the data
C     (<) DEST      (Real array (NX,NY)) The destination array
C     (>) NX        (Integer) The first dimension of DEST
C     (>) NY        (Integer) The second dimension of DEST
C     (>) X         (Integer) The column of DEST into which the data is moved
C
C                                                  DJA / AAO 16th Aug 1987
C     Modified:
C
C     2nd  Feb 1991. SCALE parameter added. KS/AAO.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N,NX,NY,X
      REAL    LINE(N), SCALE, DEST(NX,NY)
C
C     Local variables
C
      INTEGER I
C
C     Transfer data one number at a time
C
      IF (SCALE.EQ.1.0) THEN
         DO I=1,N
            DEST(X,I)=LINE(I)
         END DO
      ELSE
         DO I=1,N
            DEST(X,I)=LINE(I)*SCALE
         END DO
      END IF
      END
