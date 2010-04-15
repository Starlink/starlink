C+
C                        D S A _ R E S H A P E _ D A T A
C
C  Routine name:
C     DSA_RESHAPE_DATA
C
C  Function:
C     Creates data arrays with modified dimensions in a structure.
C
C  Description:
C     This routine produces data arrays with specified dimensions.  With
C     the exception of the dimensions, the data structure will be the
C     same as that for an existing specified structure.  This routine
C     does not modify the axis data for the structure - DSA_RESHAPE_AXIS
C     must be used for that.  This routine will modify the shapes of any
C     arrays in the model structure whose function it understands; if it
C     finds other arrays it will reshape them if they are the same shape
C     as the main data array, but will issue warning messages.  The
C     contents of the resulting data arrays are undefined if the array's
C     dimensions were changed.  If the dimensions of an array are left
C     unchanged, the array will just be copied.  The structure
C     being created may already have data arrays, in which case they will
C     be lost.  The structure being created and the structure being
C     used as a model may be the same.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_RESHAPE_DATA (REF_NAME,MODEL_NAME,NDIM,DIMS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name
C                       associated with the structure.
C     (>) MODEL_NAME    (Fixed string,descr) The reference name
C                       associated with the structure whose data is to
C                       serve as model for the reshaped structure.
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
C     ICH_FOLD, ICH_LEN, DSA_ARRAY_SIZE, DSA_DATA_SIZE, DSA_FIND_REF,
C     DSA_COERCE_DATA_ARRAY, DSA_CORECE_ARRAY, DSA_MAIN_SIZE, DSA_WRUSER,
C     DSA_WRNAME, DSA__DATA_ENV_NAME, DSA__CLEAR_DATA_ENV, DSA__ARRAY,
C     DSA__NTH_DATA_ITEM, DTA_CYVAR, DTA_CRVAR, DTA_CRNAM, DTA_DLVAR,
C     DTA_ERROR, DTA_STRUC, DTA_TYVAR
C
C  Prior requirements:
C     Both structures must have been opened by, for example, DSA_INPUT
C     or DSA_OUTPUT.  Usually, the structure being shaped will have been
C     created without data arrays.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 12th February 1995
C-
C  Subroutine / function details:
C     ICH_FOLD           Convert string to upper case
C     ICH_LEN            Position of last non-blank character in string
C     DSA_ARRAY_SIZE     Get dimensions of an array
C     DSA_FIND_REF       Look up reference name in common tables
C     DSA_COERCE_ARRAY   Force an array to a given size and type
C     DSA_COERCE_DATA_ARRAY Force the main data array to a given size & type
C     DSA_MAIN_SIZE      Get/set main array data size in common tables
C     DSA_WRUSER         Output message to user
C     DSA_WRNAME         Output name of structure element
C     DSA__CLEAR_DATA_ENV   Clear out all data arrays etc from structure
C     DSA__DATA_ENV_NAME Get name of environment structure for main data array
C     DSA__NTH_DATA_ITEM Get name of nth component of data structure
C     DSA__ARRAY         See if a named object is a data array (prim or struct)
C     DTA_CYVAR          Copy a data object
C     DTA_CRVAR          Create a data object
C     DTA_CRNAM          Create the name of a data object
C     DTA_DLVAR          Delete a data object
C     DTA_ERROR          Get text describing a DTA error code
C     DTA_STRUC          Determine if an object is a structure
C     DTA_TYVAR          Get type of object
C
C  Common variable details:
C     (<) DTA_CODE    (Integer) Last DTA_ system failure code
C     (<) SHAPE_CHECK (Logical array) Indicates data/axis shape changed.
C     (<) DATA_RESHAPE(Logical array) Indicates that the main data array
C                     was reshaped.
C
C  History:
C     28th June 1988  Original version.  KS / AAO.
C     11th Dec  1989  Reshape flag now set.  KS/AAO.
C     26th Feb  1990  Modified to use DSA__ routines to get structure
C                     details, instead of assuming the original Figaro
C                     data structure.  KS/AAO.
C     5th  Mar  1990  Bug where a non-existent array was being copied
C                     fixed.  KS/AAO.
C     26th Feb  1991  Test for NDIM=0 added. DIMS now dimensioned as '*'
C                     instead of using MAX. KS/AAO.
C     30th Mar  1991  Tests for mismatch of file types, although all it
C                     does is signal an error instead of handling the case
C                     properly. KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992   Remove unused variable declarations. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     12th Feb 1995   Now handles NDF quality structures properly. KS/AAO.
C     20th Jul 1995   Remove duplicate declarations. HME/UoE, Starlink.
C+
      SUBROUTINE DSA_RESHAPE_DATA (REF_NAME,MODEL_NAME,NDIM,DIMS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NDIM, DIMS(*), STATUS
      CHARACTER*(*) REF_NAME, MODEL_NAME
C
C     Functions used
C
      LOGICAL DSA__ARRAY
      INTEGER ICH_FOLD, ICH_LEN
C
C     Local variables
C
      INTEGER   AR_STATUS           ! Indicates if element is an array
      LOGICAL   CHANGE              ! Indicates change in dimensions
      INTEGER   DTA_STATUS          ! Status from a DTA routine
      CHARACTER ERROR*64            ! DTA error description
      LOGICAL   EXIST               ! Indicates nth data item exists
      INTEGER   I                   ! General loop index
      INTEGER   ILIM                ! # dimensions matching data array
      INTEGER   IGNORE              ! Ignored status value
      CHARACTER IN_STRUCT*80        ! Input main data structure name
      INTEGER   INVOKE              ! Dummy function value
      INTEGER   IPOS                ! Element number in data structure
      INTEGER   LENMOD              ! Number of characters in MOD_NAME
      INTEGER   LENOBJ              ! Number of characters in OBJ_NAME
      INTEGER   MOD_DIM             ! # Dimensions in model data
      INTEGER   MOD_DIMS(10)        ! Dimensions of model data
      CHARACTER MOD_NAME*80         ! DTA object name for MODEL_NAME
      CHARACTER MOD_TYPE*16         ! Type of MODEL_NAME data object
      CHARACTER MODEL_NAME_UC*32    ! Upper case version of MODEL_NAME
      INTEGER   MODEL_SLOT          ! Table slot number for model name
      CHARACTER NAME*16             ! Name of data structure element
      INTEGER   NEWDIM              ! # Dimensions in new structure element
      INTEGER   NEWDIMS(10)         ! Dimensions of new structure element
      INTEGER   OBJDIM              ! # Dimensions in structure element
      INTEGER   OBJDIMS(10)         ! Dimensions of structure element
      CHARACTER OBJECT*80           ! Full element name in structure
      CHARACTER OBJ_NAME*80         ! DTA object name for REF_NAME
      CHARACTER OUT_STRUCT*80       ! Output main data structure name
      CHARACTER QUALITY_NAME*80     ! Name of a quality array in structure.
      LOGICAL   QUALITY_STRUCT      ! True if we have a quality structure
      CHARACTER REF_NAME_UC*32      ! Upper case version of REF_NAME
      INTEGER   REF_SLOT            ! Table slot number for ref name
      LOGICAL   SAME                ! Indicates data structures are same
      CHARACTER TARGET_NAME*80      ! Full name of output structure element
      CHARACTER TYPE*16             ! Type of data structure element
      LOGICAL   USE                 ! Indicates item is associated with data
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
      IF (STATUS.NE.0) GO TO 500
      MODEL_NAME_UC=MODEL_NAME
      INVOKE=ICH_FOLD(MODEL_NAME_UC)
      CALL DSA_FIND_REF(MODEL_NAME_UC,MODEL_SLOT,MOD_NAME,LENMOD,
     :                                                        STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Check for NDIM invalid.
C
      IF (NDIM.LE.0) THEN
         CALL DSA_WRUSER(
     :              'Attempt to redimension the main data array in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,IN_STRUCT,IGNORE)
         CALL DSA_WRUSER(IN_STRUCT(:ICH_LEN(IN_STRUCT)))
         CALL DSA_WRUSER(' to zero or negative dimensions.')
         CALL DSA_WRFLUSH
         STATUS=DSA__INVDIM
         GO TO 500
      END IF
C
C     One test first.  If the input and output data arrays are the same,
C     and the dimensions are unchanged, then we don't want to do anything.
C     In any case, we'll need the array sizes and the 'same' information
C     later.
C
      SAME=OBJ_NAME.EQ.MOD_NAME
      CALL DSA_MAIN_SIZE(MODEL_SLOT,.FALSE.,10,MOD_DIM,MOD_DIMS,
     :                                                    ERROR,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CHANGE=MOD_DIM.NE.NDIM
      IF (.NOT.CHANGE) THEN
         DO I=1,NDIM
            IF (MOD_DIMS(I).NE.DIMS(I)) CHANGE=.TRUE.
         END DO
      END IF
      IF (SAME.AND.(.NOT.CHANGE)) GO TO 500
C
C     Get the type of the model object
C
      CALL DTA_TYVAR(MOD_NAME(:LENMOD),MOD_TYPE,DTA_STATUS)
C
C     At this point, we have one potential problem to trap.  If the files
C     have different formats (one .DST, the other NDF, for example) then
C     just copying items from one structure to another won't help.  What
C     we should do is do the whole thing at high level (as DSA_RESHAPE_AXIS
C     does).  Here, all we do is signal that we can't handle the problem.
C     Obviously, this ought to be fixed sometime.
C
      IF (NDF_FORMAT(REF_SLOT).NEQV.NDF_FORMAT(MODEL_SLOT)) THEN
         CALL DSA_WRUSER(
     :              'Cannot reshape the main data array in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,IN_STRUCT,IGNORE)
         CALL DSA_WRUSER(IN_STRUCT(:ICH_LEN(IN_STRUCT)))
         CALL DSA_WRUSER(' using the main data array in ')
         CALL DSA_GET_ACTUAL_NAME(MODEL_NAME,IN_STRUCT,IGNORE)
         CALL DSA_WRUSER(IN_STRUCT(:ICH_LEN(IN_STRUCT)))
         CALL DSA_WRUSER(' as a model.  The two file formats ')
         CALL DSA_WRUSER('need to be the same for this.')
         CALL DSA_WRFLUSH
         STATUS=DSA__OBJINV
         GO TO 500
      END IF
C
C     See if the model object is itself a data array, rather than
C     a structure.
C
      IF (DSA__ARRAY(MOD_NAME(:LENMOD))) THEN
C
C        The model object is a data array.  This means that all we have
C        to do is create a single output array, modeled on the input
C        array.
C
         IF (DSA__ARRAY(OBJ_NAME(:LENOBJ))) THEN
C
C           The model is a data array, and so is the target.
C
            CALL DSA_COERCE_ARRAY(OBJ_NAME,MOD_TYPE,NDIM,DIMS,STATUS)
         ELSE
C
C           Here, model is a data array, but the target is not.  The
C           simplest way of dealing with this is to realise that this
C           case can be handled by DSA_CORECE_DATA_ARRAY.
C
            CALL DSA_COERCE_DATA_ARRAY(REF_NAME,MOD_TYPE,
     :                                             NDIM,DIMS,STATUS)
         END IF
      ELSE
C
C        The model is a structure.  So we do this by looking through
C        its data structure, reshaping any arrays we find.  We
C        only do this to one level, unless we find any contracted
C        structures.
C
C        Generate the input and output structure names.
C
         CALL DSA__DATA_ENV_NAME (MODEL_SLOT,IN_STRUCT,LENMOD)
         CALL DSA__DATA_ENV_NAME (REF_SLOT,OUT_STRUCT,LENOBJ)
C
C        We need to be a little bit careful, because the two structures
C        may in fact be the same, and if they are, we don't want to
C        start by deleting the target structure!  However, if they
C        aren't, then we should start out by clearing out the target
C        - ignoring status, because it probably never existed.
C
         IF (.NOT.SAME) CALL DSA__CLEAR_DATA_ENV (REF_SLOT,IGNORE)
         CALL DTA_TYVAR(IN_STRUCT,TYPE,IGNORE)
         CALL DTA_CRVAR(OUT_STRUCT,TYPE,IGNORE)
C
C        Work through the input structure, looking for arrays to
C        reshape.  Note that DSA__NTH_DATA_ITEM logs a warning for
C        items it doesn't recognise, if it the format is a well
C        defined one and it can be sure the item shouldn't be there. Also,
C        this code does assume that all the data-type arrays will be in
C        the same environment as the main array itself - this is true of
C        the original Figaro format and the NDF format, thank goodness,
C        but this won't be generally true. Actually, it isn't true of
C        an NDF quality information structure, which we handle separately
C        here - strictly perhaps this bit should be in a DSA__ routine.
C
         IPOS=0
         EXIST=.TRUE.
         DO WHILE (EXIST)
            IPOS=IPOS+1
            CALL DSA__NTH_DATA_ITEM (MODEL_SLOT,IN_STRUCT,IPOS,EXIST,
     :                                                        USE,NAME)
            IF (EXIST.AND.USE) THEN
               CHANGE=.FALSE.
               CALL DTA_CRNAM(IN_STRUCT,NAME,0,0,OBJECT,DTA_STATUS)
               AR_STATUS=0
               QUALITY_STRUCT=.FALSE.
               CALL DSA_ARRAY_SIZE(OBJECT,10,OBJDIM,OBJDIMS,ERROR,
     :                                                   AR_STATUS)
               IF ((AR_STATUS.NE.0).AND.(NAME.EQ.'QUALITY')) THEN
C
C                 Some quality structures (NDF for example, in some cases)
C                 can have the real quality array one level down in the
C                 structure, in .QUALITY.QUALITY. We check for this
C                 one exceptional case.
C
                  AR_STATUS=0
                  CALL DTA_CRNAM(OBJECT,'QUALITY',0,0,QUALITY_NAME,
     :                                                   DTA_STATUS)
                  CALL DSA_ARRAY_SIZE(QUALITY_NAME,10,OBJDIM,OBJDIMS,
     :                                               ERROR,AR_STATUS)
                  IF (AR_STATUS.EQ.0) QUALITY_STRUCT=.TRUE.
C
               END IF
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
C              changed the order being searched by DSA__NTH_DATA_ITEM.  So
C              we step back one position before retrying.  This may mean that we
C              look again at the array we've just changed, but that doesn't
C              matter - this time we'll leave it alone. The case where we
C              have a quality structure with the array one level lower
C              down has to be handled separately, copying the structure
C              and then coercing the array within it.
C
               CALL DTA_CRNAM(OUT_STRUCT,NAME,0,0,TARGET_NAME,
     :                                                     DTA_STATUS)
               IF (CHANGE.AND..NOT.QUALITY_STRUCT) THEN
                  CALL DTA_TYVAR(OBJECT,TYPE,DTA_STATUS)
                  CALL DSA_COERCE_ARRAY(TARGET_NAME,TYPE,NEWDIM,
     :                                                 NEWDIMS,STATUS)
                  IF (STATUS.NE.0) GO TO 500
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
                        GO TO 500
                     END IF
                  END IF
                  IF (QUALITY_STRUCT) THEN
                     CALL DTA_TYVAR(QUALITY_NAME,TYPE,DTA_STATUS)
                     TARGET_NAME(ICH_LEN(TARGET_NAME)+1:)='.QUALITY'
                     CALL DSA_COERCE_ARRAY(TARGET_NAME,TYPE,NEWDIM,
     :                                                 NEWDIMS,STATUS)
                     IF (STATUS.NE.0) GO TO 500
                     IF (SAME) IPOS=IPOS-1
                  END IF
               END IF
            END IF
         END DO
      END IF
C
C     On the way out, set the new data size in the common tables, and
C     flag that the it has been modified.
C
      CALL DSA_MAIN_SIZE(REF_SLOT,.TRUE.,10,NDIM,DIMS,ERROR,STATUS)
      SHAPE_CHECK(REF_SLOT)=.TRUE.
      DATA_RESHAPE(REF_SLOT)=.TRUE.
      IF (SAME) THEN
         CALL DSA_MAIN_SIZE(MODEL_SLOT,.TRUE.,10,NDIM,DIMS,ERROR,STATUS)
         SHAPE_CHECK(MODEL_SLOT)=.TRUE.
         DATA_RESHAPE(MODEL_SLOT)=.TRUE.
      END IF
C
  500 CONTINUE
C
      END
