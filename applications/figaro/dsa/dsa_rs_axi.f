C+
C                        D S A _ R E S H A P E _ A X I S
C
C  Routine name:
C     DSA_RESHAPE_AXIS
C
C  Function:
C     Creates axis arrays with modified dimensions in a structure.
C
C  Description:
C     This routine produces axis arrays with specified dimensions.  With
C     the exception of the dimensions, the axis structure will be the
C     same as that for an existing specified axis structure.  This routine
C     will modify the shapes of any arrays in the model structure whose
C     function it thinks it understands; if it finds other arrays it will
C     reshape them if they are the same shape as the main axis data array,
C     but will issue warning messages.  The contents of any resulting data
C     arrays whose dimensions have changed are undefined, but any whose
C     dimensions are unchanged will just have been copied.  The structure
C     being created may already have data arrays, in which case they will
C     be lost.  The structure being created and the structure being used
C     as a model may be the same.  If the model structure in fact has no
C     information for the specified axis, then nor will the resulting
C     structure - but this is not regarded as an error.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_RESHAPE_AXIS (REF_NAME, AXIS, MODEL_NAME, MODEL_AXIS,
C                                                   NDIM, DIMS, STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name
C                       associated with the axis structure.
C     (>) AXIS          (Integer, ref) The number of the axis to be
C                       reshaped.
C     (>) MODEL_NAME    (Fixed string,descr) The reference name
C                       associated with the structure whose axis data is
C                       to serve as model for the reshaped structure.
C     (>) MODEL_AXIS    (Integer, ref) The number of the axis in the model
C                       structure that is to serve as the model for the
C                       reshaped axis.
C     (>) NDIM          (Integer,ref) The number of dimensions for
C                       the reshaped data.
C     (>) DIMS          (Integer array,ref) The dimensions of the
C                       reshaped data.
C     (!) STATUS        (Integer,ref) Status code. If a bad status is
C                       passed, this routine will return immediately.
C
C  External variables used:
C    Only common variables used internally by the DSA routines.
C
C  External subroutines / functions used:
C     CNF_PVAL, GEN_NTH, ICH_CI, ICH_FOLD, ICH_LEN, DSA_ARRAY_SIZE,
C     DSA_AXIS_SIZE, GEN_MOVE, DSA_PREFERRED_TYPE, DSA_UNMAP,
C     DSA_AXIS_TYPE, DSA_MAP_AXIS_DATA, DSA_SEEK_WIDTH,
C     DSA_AXIS_WIDTH_TYPE, DSA_MAP_WIDTH, DSA_GET_AXIS_INFO,
C     DSA_SET_AXIS_INFO, DSA_SET_WIDTH, DSA_TYPESIZE,  DSA_FIND_REF,
C     DSA_GET_ACTUAL_NAME, DSA_COERCE_ARRAY, DSA_WRUSER,
C     DSA_VALIDATE_ARRAY, DSA__CREATE_AXIS, DSA__AXIS_NAME,
C     DSA__DELETE_AXIS, DSA_WRNAME, DTA_CYVAR, DTA_CRVAR, DTA_CRNAM,
C     DTA_DLVAR, DTA_ERROR, DTA_NMVAR, DTA_STRUC, DTA_TYVAR
C
C  Prior requirements:
C     Both structures must have been opened by, for example, DSA_INPUT
C     or DSA_OUTPUT.  Usually, the structure being shaped will have been
C     created without axis data arrays.  The main data array for the
C     resulting structure must already exist (so that AXIS can be
C     validated).
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     CNF_PVAL           Full pointer to dynamically allocated memory
C     GEN_NTH            Returns 'st','th','rd' etc appropriate to a number.
C     GEN_MOVE           Copies an array of bytes
C     ICH_CI             Return an integer as a character string.
C     ICH_FOLD           Convert string to upper case
C     ICH_LEN            Position of last non-blank character in string
C     DSA_ARRAY_SIZE     Get dimensions of an array
C     DSA_AXIS_SIZE      Get dimensions of an axis data array
C     DSA_TYPESIZE       Size of an array element of given type in bytes
C     DSA_PREFERRED_TYPE Raw data type suitable for data of structured type
C     DSA_MAP_AXIS_DATA  Map main axis data array
C     DSA_MAP_WIDTH      Map axis width array
C     DSA_SEEK_WIDTH     See if an axis width array exists
C     DSA_AXIS_WIDTH_TYPE Get type of axis width array
C     DSA_AXIS_TYPE      Get type of main axis data array
C     DSA_GET_AXIS_INFO  Get units, label, etc information about axis
C     DSA_SET_AXIS_INFO  Set units, label, etc information about axis
C     DSA_FIND_REF       Look up reference name in common tables
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C     DSA_COERCE_ARRAY   Force an array to a given size and type
C     DSA_VALIDATE_AXIS  Check that a given axis number is valid.
C     DSA__CREATE_AXIS   Ensure that a given axis structure exists.
C     DSA__DELETE_AXIS   Clear out an axis structure
C     DSA__AXIS_NAME     Get DTA system name for a given axis structure.
C     DSA_WRUSER         Output message to user
C     DSA_WRNAME         Output name of structure element
C     DTA_CYVAR          Copy a data object
C     DTA_CRVAR          Create a data object
C     DTA_CRNAM          Create the name of a data object
C     DTA_DLVAR          Delete a data object
C     DTA_ERROR          Get text describing a DTA error code
C     DTA_NMVAR          Get name of nth object in a structure
C     DTA_STRUC          Determine if an object is a structure
C     DTA_TYVAR          Get type of object
C
C  Common variable details:
C     (<) DTA_CODE    (Integer) Last DTA_ system failure code
C     (<) SHAPE_CHECK (Logical array) Indicates data/axis shape changed.
C     (<) AXIS_RESHAPE(Logical array) Indicates that the axis data structures
C                     were reshaped.
C     (<) AXIS_EXIST  (Integer array) State of knowledge about axis arrays.
C                     Indicates unknown (0), known not to exist (-1),
C                     exists and is 1d (1), exists and is multi-
C                     dimensional (2).  (2D array)
C     (>) NDF_FORMAT  (Logical array) Indicates structure format is Starlink's
C                     NDF format (described in SGP38).  If false, format is
C                     original Figaro format (DST files).
C
C  History:
C     1st  Jul 1988  Original version.  KS / AAO.
C     18th Jul 1988  Check that input axis exists added.  KS/AAO.
C     11th Dec 1989  Sets axis-reshaped flag.  KS/AAO.
C     2nd  Mar 1990  Modified to use DSA__ routines rather than assuming
C                    the original Figaro format.  KS/AAO.
C     7th  Mar 1990  Now clears the common AXIS_EXIST flag. KS/AAO.
C     24th Mar 1991  Handle case where data file types are incompatible. KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     15th Oct 1992  HME / UoE, Starlink.  Refuse creation of an N-D
C                    axis in an NDF.
C     13th Feb 1995  Previous change reversed. N-D arrays are now allowed to
C                    be created in an NDF, but will be moved to the extension
C                    structure when the structure is closed. Code now checks
C                    that it has in fact reshaped the data array. KS/AAO.
C     2005 June 3    Replace DYNAMIC_MEMORY with %VAL(CNF_PVAL(ADDRESS))
C                    contruct for 64-bit addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_RESHAPE_AXIS (REF_NAME,AXIS,MODEL_NAME,MODEL_AXIS,
     :                                                NDIM,DIMS,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      INTEGER AXIS, MODEL_AXIS, NDIM, DIMS(*), STATUS
      CHARACTER*(*) REF_NAME, MODEL_NAME
C
C     Functions used
C
      INTEGER DSA_TYPESIZE, ICH_FOLD, ICH_LEN
      CHARACTER GEN_NTH*2, ICH_CI*12
C
C     Local variables
C
      INTEGER   AR_STATUS        ! Indicates if element is an array
      INTEGER   BYTES            ! Number of bytes to copy to new array
      LOGICAL   CHANGE           ! Indicates change in dimensions
      INTEGER   COUNT            ! Loop counter
      INTEGER   DTA_STATUS       ! Status from a DTA routine
      CHARACTER ERROR*64         ! DTA error description
      LOGICAL   EXIST            ! True if structure element exists
      INTEGER   I                ! General loop index
      INTEGER   ILIM             ! No. of dimensions matching data array
      INTEGER   IGNORE           ! Ignored status value
      CHARACTER IN_STRUCT*80     ! Input main data structure name
      INTEGER   INVOKE           ! Dummy function value
      INTEGER   IPOS             ! Element number in data structure
      INTEGER   LENGTH           ! Number of characters in axis name
      INTEGER   LENMOD           ! Number of characters in MOD_NAME
      INTEGER   LENOBJ           ! Number of characters in OBJ_NAME
      INTEGER   MOD_ADDR         ! Virtual address of mapped model array
      INTEGER   MOD_DATA_SLOT    ! Map slot used for model array
      INTEGER   MOD_DIM          ! No. of dimensions in model data
      INTEGER   MOD_DIMS(10)     ! Dimensions of model data
      CHARACTER MOD_NAME*80      ! DTA object name for MODEL_NAME
      INTEGER   MOD_NELM         ! Number of elements in model axis data
      CHARACTER MODEL_NAME_UC*32 ! Upper case version of MODEL_NAME
      INTEGER   MODEL_SLOT       ! Table slot number for model name
      CHARACTER NAME*16          ! Name of data structure element
      INTEGER   NELM             ! Number of elements in axis array
      INTEGER   NEWDIM           ! # Dimensions in new structure element
      INTEGER   NEWDIMS(10)      ! Dimensions of new structure element
      INTEGER   NM_STATUS        ! Status returned by DTA_NMVAR
      CHARACTER NUMBER*12        ! Formatted number
      INTEGER   OBJDIM           ! # Dimensions in structure element
      INTEGER   OBJDIMS(10)      ! Dimensions of structure element
      CHARACTER OBJECT*80        ! Full element name in structure
      CHARACTER OBJ_NAME*80      ! DTA object name for REF_NAME
      CHARACTER OUT_STRUCT*80    ! Output main data structure name
      INTEGER   REF_ADDR         ! Virtual address of mapped new array
      INTEGER   REF_DATA_SLOT    ! Map slot used for new array
      CHARACTER REF_NAME_UC*32   ! Upper case version of REF_NAME
      INTEGER   REF_SLOT         ! Table slot number for ref name
      LOGICAL   SAME             ! Indicates data structures are same
      LOGICAL   SINGLE           ! True if width array is single valued
      CHARACTER STRINGS(2)*64    ! Axis label and units
      LOGICAL   STRUCT           ! True if array is of structured type
      CHARACTER STRUCT_TYPE*16   ! Type of structured array
      CHARACTER STRUCTURE_NAME*128  ! Full structure name from ref_name
      CHARACTER TARGET_NAME*80   ! Full name of output structure element
      INTEGER   TEST_AXIS        ! Axis being tested for validity
      CHARACTER TEST_NAME*32     ! Structure being tested
      CHARACTER TYPE*16          ! Type of data structure element
      DOUBLE PRECISION VALUES(1) ! Numeric information array for axis
      DOUBLE PRECISION WIDTH     ! Single width value for axis
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     DSA system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Immediate return if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Convert reference names to upper case and look them up.
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF(REF_NAME_UC,REF_SLOT,OBJ_NAME,LENOBJ,STATUS)
      IF (STATUS.NE.0) GO TO 500   ! Error exit
      MODEL_NAME_UC=MODEL_NAME
      INVOKE=ICH_FOLD(MODEL_NAME_UC)
      CALL DSA_FIND_REF(MODEL_NAME_UC,MODEL_SLOT,MOD_NAME,LENMOD,
     :                                                        STATUS)
      IF (STATUS.NE.0) GO TO 500   ! Error exit
C
C     If we're going to make this sort of change to an axis, we should
C     reset the `known' flag for it.
C
      AXIS_EXIST(AXIS,REF_SLOT)=0
C
C     We validate the value of AXIS and MODEL_AXIS first.
C
      CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME_UC,STATUS)
      CALL DSA_VALIDATE_AXIS (MODEL_AXIS,MODEL_NAME_UC,STATUS)
      IF (STATUS.NE.0) GO TO 500   ! Error exit
      DO COUNT=1,2
         IF (COUNT.EQ.1) THEN
            TEST_AXIS=AXIS
            TEST_NAME=REF_NAME_UC
         ELSE
            TEST_AXIS=MODEL_AXIS
            TEST_NAME=MODEL_NAME_UC
         END IF
         IF (STATUS.NE.0) GO TO 500        ! Error exit
         CALL DSA_DATA_SIZE(TEST_NAME,10,OBJDIM,OBJDIMS,NELM,STATUS)
         IF (TEST_AXIS.GT.OBJDIM) THEN
            NUMBER=ICH_CI(TEST_AXIS)
            CALL DSA_WRUSER('Unable to access the ')
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER))//
     :                                          GEN_NTH(TEST_AXIS))
            CALL DSA_WRUSER(' axis of the structure ')
            CALL DSA_GET_ACTUAL_NAME(TEST_NAME,STRUCTURE_NAME,STATUS)
            CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
            CALL DSA_WRUSER(
     :               ' The data does not have this many dimensions.')
            CALL DSA_WRUSER(' Probable programming error.')
            CALL DSA_WRFLUSH
            STATUS=DSA__AXINV
            GO TO 500    ! Error exit
         END IF
      END DO
C
C     If the input and output axis arrays are the same, and the dimensions
C     are unchanged, then we don't want to do anything.  In any case, we'll
C     need the array sizes and the 'same' information later.
C
      SAME=OBJ_NAME.EQ.MOD_NAME
      CALL DSA_AXIS_SIZE(MODEL_NAME,MODEL_AXIS,10,MOD_DIM,MOD_DIMS,
     :                                                 MOD_NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500  ! Error exit
      CHANGE=MOD_DIM.NE.NDIM
      IF (.NOT.CHANGE) THEN
         DO I=1,NDIM
            IF (MOD_DIMS(I).NE.DIMS(I)) CHANGE=.TRUE.
         END DO
      END IF
      IF (SAME.AND.(.NOT.CHANGE)) GO TO 500   ! Everything done
C
C     The model axis has to be a structure.  So we look through
C     its data structure, reshaping any arrays we find.  We
C     only do this to one level, unless we find any contracted
C     structures.
C
C     Generate the input and output structure names.
C
      CALL DSA__AXIS_NAME (MODEL_SLOT,MODEL_AXIS,IN_STRUCT,LENGTH)
      CALL DSA__AXIS_NAME (REF_SLOT,AXIS,OUT_STRUCT,LENGTH)
C
C     Allow for the possibility that the input structure doesn't exist
C     at all, in which case we don't have to do anything.  We use TYPE
C     later.
C
      CALL DTA_TYVAR(IN_STRUCT,TYPE,DTA_STATUS)
      IF (DTA_STATUS.NE.0) GO TO 500     ! All done
C
C     At this point, we have one potential problem to trap.  If the files
C     have different formats (one .DST, the other NDF, for example) then
C     we have to handle things differently, at a higher level.  So here
C     we check that that isn't the case.
C
      IF (NDF_FORMAT(REF_SLOT).EQV.NDF_FORMAT(MODEL_SLOT)) THEN
C
C        There is one remaining optimisation:  if the dimensions are not being
C        changed, then we do the whole operation in a single structure copy.
C        (The delete axis followed by create axis sequence clears out the axis,
C        although it looks a bit heavy-handed)
C
         IF (.NOT.CHANGE) THEN
            CALL DSA__DELETE_AXIS (REF_SLOT,AXIS,IGNORE)
            CALL DSA__CREATE_AXIS (REF_SLOT,AXIS,IGNORE)
            CALL DTA_CYVAR(IN_STRUCT,OUT_STRUCT,DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               CALL DSA_WRUSER('Error while copying ')
               CALL DSA_WRNAME(IN_STRUCT)
               CALL DSA_WRUSER(' to ')
               CALL DSA_WRNAME(OUT_STRUCT)
               CALL DSA_WRUSER('. ')
               CALL DTA_ERROR(DTA_STATUS,ERROR)
               CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER('.')
               CALL DSA_WRFLUSH
               DTA_CODE=DTA_STATUS
               STATUS=DSA__DTAERR
               GO TO 500    ! Error exit
            END IF
            GO TO 500    ! All done.
         END IF
C
C        We need to be a little bit careful, because the two structures
C        may in fact be the same, and if they are, we don't want to
C        start by deleting the target structure!  However, if they
C        aren't, then we should start out by clearing out the target
C        - ignoring status, because it probably never existed.
C
         IF (.NOT.SAME) CALL DSA__DELETE_AXIS (REF_SLOT,AXIS,IGNORE)
         CALL DSA__CREATE_AXIS (REF_SLOT,AXIS,IGNORE)
C
C        The following code is very similar to that used by DSA_RESHAPE_DATA,
C        except that that uses DSA__NTH_DATA_ITEM whereas this routine can get
C        away with using DTA_NMVAR (assuming that the file formats supported
C        all have the axis arrays in separate structures).  There may be some
C        scope for recoding this as a subroutine both routines could use.
C
C        Work through the input structure
C
         IPOS=0
         NM_STATUS=0
         DO WHILE (NM_STATUS.EQ.0)
            IPOS=IPOS+1
            CALL DTA_NMVAR(IN_STRUCT,IPOS,NAME,NM_STATUS)
            IF (NM_STATUS.EQ.0) THEN
               CHANGE=.FALSE.
               CALL DTA_CRNAM(IN_STRUCT,NAME,0,0,OBJECT,DTA_STATUS)
               AR_STATUS=0
               CALL DSA_ARRAY_SIZE(OBJECT,10,OBJDIM,OBJDIMS,ERROR,
     :                                                   AR_STATUS)
               IF ((AR_STATUS.EQ.0).AND.(OBJDIM.NE.0)) THEN
C
C                 The object we've found is an array (either contracted
C                 or primitive).  Now, whether we want to change it is
C                 a question of how its dimensions compare with those of
C                 the main data array.  (If we knew exactly what objects
C                 we expected to find in the data structure, we could be
C                 a little more precise about this test, but the old
C                 Figaro format was a bit too slack for that.)  The code
C                 that follows attempts to be sensible about deciding
C                 whether or not the dimensions of an array should be
C                 changed.  Essentially, it says that any dimensions of
C                 the existing array that match the dimensions of the
C                 main data array should be replaced by the new dimensions
C                 specified in the reshape.
C
                  ILIM=OBJDIM
                  DO I=1,MIN(OBJDIM,MOD_DIM)
                     IF (OBJDIMS(I).NE.MOD_DIMS(I)) THEN
                        ILIM=I-1
                        GO TO 340       ! Break I loop
                     END IF
                  END DO
  340             CONTINUE
                  IF (ILIM.LT.MOD_DIM) THEN
                     NEWDIM=ILIM
                  ELSE
                     NEWDIM=NDIM
                  END IF
                  DO I=1,NEWDIM
                     NEWDIMS(I)=DIMS(I)
                  END DO
                  DO I=ILIM+1,OBJDIM
                     NEWDIM=NEWDIM+1
                     NEWDIMS(NEWDIM)=OBJDIMS(I)
                  END DO
                  CHANGE=NEWDIM.NE.OBJDIM
                  IF (.NOT.CHANGE) THEN
                     DO I=1,NEWDIM
                        IF (NEWDIMS(I).NE.OBJDIMS(I)) CHANGE=.TRUE.
                     END DO
                  END IF
               END IF
C
C              Now, if we need to change the array, we use DSA_COERCE_ARRAY
C              to modify it.  Note the subtlety that if we change an array
C              in situ (ie if the input and output structures are the same)
C              then we may find we've removed the original array and so
C              changed the order being searched by DTA_NMVAR.  So we step
C              back one position before retrying.  This may mean that we
C              look again at the array we've just changed, but that doesn't
C              matter - this time we'll leave it alone.
C
               CALL DTA_CRNAM(OUT_STRUCT,NAME,0,0,TARGET_NAME,
     :                                                     DTA_STATUS)
               IF (CHANGE) THEN
                  CALL DTA_TYVAR(OBJECT,TYPE,DTA_STATUS)
                  CALL DSA_COERCE_ARRAY(TARGET_NAME,TYPE,NEWDIM,
     :                                                 NEWDIMS,STATUS)
                  IF (STATUS.NE.0) GO TO 500      ! Error exit
                  IF (SAME) IPOS=IPOS-1
               ELSE
C
C                 If the data object isn't to be changed, it will
C                 probably have to be copied.
C
                  IF (.NOT.SAME) THEN
                     CALL DTA_CYVAR(OBJECT,TARGET_NAME,DTA_STATUS)
                     IF (DTA_STATUS.NE.0) THEN
                        CALL DSA_WRUSER('Error while copying ')
                        CALL DSA_WRNAME(OBJECT)
                        CALL DSA_WRUSER(' to ')
                        CALL DSA_WRNAME(TARGET_NAME)
                        CALL DSA_WRUSER('. ')
                        CALL DTA_ERROR(DTA_STATUS,ERROR)
                        CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                        CALL DSA_WRUSER('.')
                        CALL DSA_WRFLUSH
                        DTA_CODE=DTA_STATUS
                        STATUS=DSA__DTAERR
                        GO TO 500    ! Error exit
                     END IF
                  END IF
               END IF
            END IF
         END DO
      ELSE
C
C        This is the problem case where the two files have different
C        formats.  In this case, we can't handle the reshape by just
C        handling every item we find in the structure, but have to
C        do it at a higher level, taking each element we expect to
C        find in an axis array and copying it over.  First, we get rid
C        of any existing axis in the target structure.
C
         CALL DSA__DELETE_AXIS (REF_SLOT,AXIS,IGNORE)
C
C        We will have to copy data over explicitly, so see how many
C        data elements will have to be copied.
C
         NELM=1
         DO I=1,NDIM
            NELM=NELM*DIMS(I)
         END DO
         IF (MOD_NELM.LT.NELM) NELM=MOD_NELM
C
C        Start by copying over the main axis data array
C
         CALL DSA_AXIS_TYPE (MODEL_NAME,MODEL_AXIS,TYPE,STRUCT,STATUS)
         IF (STRUCT) THEN
            STRUCT_TYPE=TYPE
            CALL DSA_PREFERRED_TYPE (STRUCT_TYPE,TYPE,STATUS)
         END IF
         CALL DSA_MAP_AXIS_DATA (MODEL_NAME,MODEL_AXIS,'READ',TYPE,
     :                           MOD_ADDR,MOD_DATA_SLOT,STATUS)
         CALL DSA_MAP_AXIS_DATA (REF_NAME,AXIS,'WRITE',TYPE,
     :                           REF_ADDR,REF_DATA_SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500      ! Error exit
         BYTES=DSA_TYPESIZE(TYPE,STATUS)*NELM
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(MOD_ADDR)),
     :                 %VAL(CNF_PVAL(REF_ADDR)))
         CALL DSA_UNMAP(REF_DATA_SLOT,STATUS)
         CALL DSA_UNMAP(MOD_DATA_SLOT,STATUS)
C
C        Repeat this for the width array, if there is one.  If we have a
C        single width value, we can set that easily, so we check for that.
C
         CALL DSA_SEEK_WIDTH (MODEL_NAME,MODEL_AXIS,EXIST,SINGLE,
     :                        WIDTH,STATUS)
         IF (EXIST) THEN
            IF (SINGLE) THEN
               CALL DSA_SET_WIDTH (REF_NAME,AXIS,WIDTH,STATUS)
            ELSE
               CALL DSA_AXIS_WIDTH_TYPE (MODEL_NAME,MODEL_AXIS,TYPE,
     :                                   STRUCT,STATUS)
               IF (STRUCT) THEN
                  STRUCT_TYPE=TYPE
                  CALL DSA_PREFERRED_TYPE (STRUCT_TYPE,TYPE,STATUS)
               END IF
               CALL DSA_MAP_WIDTH (MODEL_NAME,MODEL_AXIS,'READ',
     :                             TYPE,MOD_ADDR,MOD_DATA_SLOT,STATUS)
               CALL DSA_MAP_WIDTH (REF_NAME,AXIS,'WRITE',TYPE,REF_ADDR,
     :                             REF_DATA_SLOT,STATUS)
               IF (STATUS.NE.0) GO TO 500      ! Error exit
               BYTES=DSA_TYPESIZE(TYPE,STATUS)*NELM
               CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(MOD_ADDR)),
     :                       %VAL(CNF_PVAL(REF_ADDR)))
               CALL DSA_UNMAP(REF_DATA_SLOT,STATUS)
               CALL DSA_UNMAP(MOD_DATA_SLOT,STATUS)
            END IF
         END IF
C
C        Then the rest of the axis information
C
         CALL DSA_GET_AXIS_INFO (MODEL_NAME,MODEL_AXIS,2,STRINGS,1,
     :                                                  VALUES,STATUS)
         CALL DSA_SET_AXIS_INFO (REF_NAME,AXIS,2,STRINGS,1,VALUES,
     :                                                         STATUS)
         IF (STATUS.NE.0) GO TO 500   ! Error exit
      END IF
C
C     This may seem silly at first sight (and second, if you ask me, but
C     I didn't design the NDF format..)  If the array is a multi-dimensional
C     array in an NDF, changed in situ, then the actual array may be in
C     an extension. In this case, we may have missed it in all the above.
C     So we make sure that we've actually got an axis data array of the right
C     size in the output struture. If we don't, we coerce it explicitly.
C
      CALL DSA__AXIS_DATA_NAME (REF_SLOT,AXIS,OBJECT,LENGTH)
      CALL DSA_ARRAY_SIZE (OBJECT(:LENGTH),10,OBJDIM,OBJDIMS,ERROR,
     :                                                         STATUS)
      CHANGE=.FALSE.
      IF (NDIM.NE.OBJDIM) THEN
         CHANGE=.TRUE.
      ELSE
         DO I=1,NDIM
            IF (DIMS(I).NE.OBJDIMS(I)) CHANGE=.TRUE.
         END DO
      END IF
      IF (CHANGE) THEN
         CALL DSA_AXIS_TYPE (REF_NAME,AXIS,TYPE,STRUCT,STATUS)
         CALL DSA_COERCE_AXIS_DATA (REF_NAME,AXIS,TYPE,NDIM,DIMS,STATUS)
      END IF
      IF (STATUS.NE.0) GO TO 500    ! Error exit
C
C     On the way out, flag that there has been a shape change made.
C
      SHAPE_CHECK(REF_SLOT)=.TRUE.
      AXIS_RESHAPE(AXIS,REF_SLOT)=.TRUE.
      IF (SAME) THEN
         SHAPE_CHECK(MODEL_SLOT)=.TRUE.
         AXIS_RESHAPE(AXIS,MODEL_SLOT)=.TRUE.
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
