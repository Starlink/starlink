C+
C               D S A _ C O E R C E _ A R R A Y
C
C  Routine name:
C     DSA_COERCE_ARRAY
C
C  Function:
C     Forces an array to a specified type and size.
C
C  Description:
C     This routine creates a named new array of specified type and
C     size.  If such an array already exists, it will replace it if
C     necessary - if the type is not that specified, or if the
C     dimensions differ.  If the array does exist, it should not be
C     mapped at the time this call is made.  The data, if any, in an
C     existing array is maintained unless the type is changed, in which
C     case it is lost completely.  If the data array merely changes
C     size, then existing data will not be lost, but it will not be
C     re-ordered in the array either, so the relation between array
C     index values and data will change unless the only change is to
C     the last dimension of the array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_COERCE_ARRAY (OBJ_NAME,TYPE,NDIM,DIMS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) OBJ_NAME      (Fixed string,descr) The DTA system name for the
C                       array to be coerced.  This should not include
C                       dimension information.
C     (>) TYPE          (Fixed string,descr) The type that the data array
C                       is to have.  This must be one of the primitive
C                       types recognised by the DSA_ routines, or one of the
C                       structure types recognised by this implementation.
C     (>) NDIM          (Integer,ref) The number of dimensions the array
C                       is to have.
C     (>) DIMS          (Integer array,ref) The dimensions the array is
C                       to have.
C     (!) STATUS        (Integer,ref) Status code.  If bad status is
C                       passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ routines
C
C  External subroutines / functions used:
C     ICH_LEN, ICH_FOLD, DSA_WRUSER, DSA_ARRAY_SIZE, DSA_CREATE_ARRAY,
C     DSA_RESHAPE_ARRAY, DSA_WRNAME, DSA_FIND_REF, DTA_ERROR, DTA_DLVAR,
C     DSA_CHECK_MAPPING, DTA_TYVAR
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ routines.
C     The file containing the array in question must have been opened, eg
C     by DSA_OUTPUT, and the environment for the array (the structure
C     that contains it) must exist, even if the array itself does not.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C  Note:
C     This is an DSA_ system internal routine and should not be
C     called directly from outside the DSA package.
C-
C  Common variable details:
C     (<) DTA_CODE      (Integer) Last DTA_ system failure status code.
C     (>) MAX_AXES      (Integer parameter) Maximum number of axes for data.
C     (>) MAX_MAPS      (Integer parameter) Maximum number of mapped arrays.
C     (!) MAP_USED      (Logical array) Indicates map slot in use.
C     (>) MAP_ACTNAM    (String array) Names of actual mapped arrays.
C     (>) MAP_COUNT     (Integer array) Reference count for this array.
C
C  Subroutine / function details:
C     ICH_LEN        Position of last non-blank character in string
C     ICH_FOLD       Convert a string to upper case
C     DSA_WRUSER     Output string to user
C     DSA_ARRAY_SIZE Gets dimensions of data array
C     DSA_CREATE_ARRAY  Creates an array of specified size and type
C     DSA_RESHAPE_ARRAY Changes the shape of an existing array
C     DSA_CHECK_MAPPING Checks for mapped arrays in a structure
C     DSA_WRNAME     Output full name for data object
C     DTA_ERROR      Get error message for DTA error code
C     DTA_DLVAR      Delete a data object
C     DTA_TYVAR      Get type of a data object
C
C  History:
C     30th June 1988  Original version.  KS / AAO.
C     4th  Oct  1989  Comments modified to describe effect on existing
C                     data correctly.  KS/AAO.
C     18th Jan  1990  Now uses DSA_CREATE_ARRAY and DSA_RESHAPE_ARRAY so
C                     this routine no longer has to know about structured
C                     array types.  KS/AAO.
C     23rd Apr  1990  Declaration of DIMS changed to allow for NDIM=0. KS/AAO.
C     27th Apr  1990  Check on mapped arrays now allows for mapped structured
C                     arrays. KS/AAO.
C     2nd  May  1990  Nows uses DSA_CHECK_MAPPING rather than doing the
C                     mapping checks in-line.  KS/AAO.
C     21st Aug  1992  Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug  1992  Remove unused variable declarations. KS/AAO
C     29th Aug  1992  Declare MATCH properly as LOGICAL not not INTEGER. KS/AAO.
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C
C+
      SUBROUTINE DSA_COERCE_ARRAY (OBJ_NAME,TYPE,NDIM,DIMS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NDIM, DIMS(*), STATUS
      CHARACTER*(*) OBJ_NAME, TYPE
C
C     Functions used
C
      INTEGER ICH_LEN, ICH_FOLD
C
C     DSA_ system common
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   AR_STATUS             ! 0 if data object is an array
      CHARACTER ARRAY_NAME*80         ! Name of actual array in structure
      CHARACTER DATA_NAME*80          ! Name of array to coerce
      INTEGER   DIMS1(MAX_AXES)       ! Dimensions of existing array
      INTEGER   DTA_STATUS            ! DTA_ system status value
      CHARACTER ERROR*64              ! DTA_ error description
      INTEGER   I                     ! Loop index
      INTEGER   IGNORE                ! Dummy status value
      INTEGER   INVOKE                ! Dummy function value
      LOGICAL   KNOWN                 ! True if structured type known - ignored
      INTEGER   LENAME                ! Length of ARRAY_NAME
      INTEGER   LENGTH                ! Length of OBJ_NAME
      LOGICAL   MATCH                 ! True if existing array matches
      INTEGER   NDIM1                 ! # Dimensions of existing array
      CHARACTER OLD_TYPE*16           ! Type of existing object
      CHARACTER TYPE_UC*16            ! Upper case version of TYPE
      CHARACTER VARIANT*16            ! Array variant if structured - ignored
C
C     Return if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     We need an upper case version of TYPE
C
      TYPE_UC=TYPE
      INVOKE=ICH_FOLD(TYPE_UC)
C
C     We start by making DATA_NAME just an upper case version
C     of OBJ_NAME
C
      DATA_NAME=OBJ_NAME
      LENGTH=ICH_FOLD(DATA_NAME)
C
C     See if object already exists.
C
      MATCH=.FALSE.
      CALL DTA_TYVAR(DATA_NAME,OLD_TYPE,DTA_STATUS)
      IF (DTA_STATUS.EQ.0) THEN
C
C        It does exist.  That's OK, it just means we probably have to
C        delete it first.  If it's the same type, see if it also has
C        the same dimensions as what we are trying to force.
C
         IF (OLD_TYPE.EQ.TYPE_UC) THEN
            CALL DSA_ARRAY_SIZE(DATA_NAME,MAX_AXES,NDIM1,DIMS1,ERROR,
     :                                                     AR_STATUS)
            IF ((AR_STATUS.EQ.0).AND.(NDIM1.NE.0)) THEN
               MATCH=NDIM.EQ.NDIM1
               IF (MATCH) THEN
                  DO I=1,NDIM
                     IF (DIMS(I).NE.DIMS1(I)) MATCH=.FALSE.
                  END DO
               END IF
            END IF
         END IF
C
C        If we had a match, then OK.  Otherwise, we can rename it if the
C        type is the same and only the dimensions are to change.  If
C        the type doesn't match, then we have to delete and recreate it.
C        One complication here is that we have to make sure it
C        isn't mapped.  This means looking through the map tables for it.
C        Remember that DSA_UNMAP only clears 'in use' flags; it doesn't
C        normally actually unmap the data.  So we may have to do that
C        ourselves.
C
         IF (.NOT.MATCH) THEN
            IGNORE=0
            CALL DSA__STRUCT_ARRAY(DATA_NAME,LENGTH,OLD_TYPE,VARIANT,
     :                                  ARRAY_NAME,LENAME,KNOWN,IGNORE)
            CALL DSA_CHECK_MAPPING(ARRAY_NAME,LENAME,
     :                                   're-shape data array ',STATUS)
            IF (STATUS.NE.0) GO TO 500   ! Error exit
C
C           Now we have made sure there are no outstanding mappings,
C           delete the object, if its type will prevent a rename.
C
            IF (TYPE_UC.NE.OLD_TYPE) THEN
               CALL DTA_DLVAR(DATA_NAME,DTA_STATUS)
               IF (DTA_STATUS.NE.0) THEN
                  CALL DSA_WRUSER('Error deleting the existing array ')
                  CALL DSA_WRNAME(DATA_NAME)
                  CALL DSA_WRUSER('. ')
                  CALL DTA_ERROR(DTA_STATUS,ERROR)
                  CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                  CALL DSA_WRUSER('.')
                  CALL DSA_WRFLUSH
                  DTA_CODE=DTA_STATUS
                  STATUS=DSA__DTAERR
                  GO TO 500        ! Error exit
               END IF
            ELSE
C
C              The array exists, and is the right type, so we can
C              rename it to the shape we want.  Then it will match.
C
               CALL DSA_RESHAPE_ARRAY(' ',DATA_NAME(:LENGTH),NDIM,DIMS,
     :                                                          STATUS)
               IF (STATUS.NE.0) GO TO 500    ! Error exit
               MATCH=.TRUE.
            END IF
         END IF
      END IF
C
C     Now, either we had an exact match with an existing array, (or managed
C     to reshape an existing array so it did match) or there is no data
C     object of that type (either because there never was, or because it
C     was deleted.)  If there was no match, create the new array.
C
      IF (.NOT.MATCH) THEN
         CALL DSA_CREATE_ARRAY (' ',DATA_NAME(:LENGTH),TYPE,NDIM,
     :                                                      DIMS,STATUS)
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
