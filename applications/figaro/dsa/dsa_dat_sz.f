C+
C
C                      D S A _ D A T A _ S I Z E
C
C  Routine name:
C     DSA_DATA_SIZE
C
C  Function:
C     Returns the dimensions of the data array.
C
C  Description:
C     This routine returns the dimensions and total number of elements
C     in the main data array of a structure.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DATA_SIZE (REF_NAME,MAX_DIM,NDIM,DIMS,ELEMENTS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) MAX_DIM      (Integer,ref) The maximum number of dimensions
C                      for the data.
C     (<) NDIM         (Integer,ref) The actual number of dimensions in
C                      the data.
C     (<) DIMS         (Integer array,ref) The number of elements in
C                      each axis of the data.  Elements DIMS(NDIM+1)
C                      to DIMS(MAX_DIM) if any, are set to 1.
C     (<) ELEMENTS     (Integer,ref) The total number of elements in the
C                      data.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, DSA_FIND_REF, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     DSA_MAIN_SIZE, DTA_STRUCT, ICH_FOLD
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_CI        Return an integer as a character string.
C     ICH_FOLD      Convert a string to upper case.
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_MAIN_SIZE Get the size of a structure's main data array.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_WRUSER    Output a message string to the user.
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C     DSA_ARRAY_EXIST  See if a named array exists in a valid form.
C     DSA__DATA_NAME   Get the name of the main data array for a structure.
C     DTA_STRUCT     See if a named object is structured.
C
C  History:
C     15th June 1987.   Original version.  KS / AAO.
C     2nd  Feb  1989.   Now ensures unused DIMS elements are set to 1.
C     8th  Dec  1989.   Comments reformatted to avoid problems when
C                       processed using MAN.  KS/AAO.
C     15th Apr  1990.   Call to DSA_ARRAY_EXIST added to improve error
C                       messages if structure is invalid.  KS/AAO.
C     23rd Apr  1990.   Above call modified to allow for possibility that
C                       the ref-name refers directly to an array. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_DATA_SIZE (REF_NAME,MAX_DIM,NDIM,DIMS,
     :                                               ELEMENTS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME
      INTEGER MAX_DIM, NDIM, DIMS(MAX_DIM), ELEMENTS, STATUS
C
C     Functions used
C
      CHARACTER ICH_CI*1
      INTEGER   ICH_FOLD, ICH_LEN
C
C     Local variables
C
      INTEGER   DARRAY(10)                  ! Dimensions of data array
      INTEGER   DTA_STATUS                  ! DTA_ error code
      CHARACTER ERROR*64                    ! DTA_ error description
      LOGICAL   EXIST                       ! True if valid array exists
      INTEGER   I                           ! Loop variable
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   IPT                         ! Start of array name in string
      INTEGER   LENGTH                      ! Object name length
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot # - ignored
      LOGICAL   STRUCT                      ! True if object is a structure
      CHARACTER STRUCTURE_NAME*128          ! Full structure name from ref_name
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Generate the name of the main data array and see if it exists.
C     Note that this isn't strictly necessary (and is even inefficient)
C     since if it doesn't exist we can tell that from the bad status
C     we'll get from DSA_MAIN_SIZE.  However, especially now that DSA
C     makes more use of structured arrays, it's more important to be able
C     to put out decent error messages if the structure isn't valid.
C
      CALL DTA_STRUC (OBJ_NAME(:LENGTH),STRUCT,DTA_STATUS)
      IF (STRUCT) CALL DSA__DATA_NAME (REF_SLOT,OBJ_NAME,LENGTH)
      CALL DSA_ARRAY_EXIST (OBJ_NAME(:LENGTH),EXIST,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (.NOT.EXIST) THEN
         CALL DSA_WRUSER(
     :      'Unable to get the dimensions of the main data array in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME_UC,STRUCTURE_NAME,IGNORE)
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         CALL DSA_WRUSER ('. The main data array (')
         IPT=INDEX(OBJ_NAME,'.')
         CALL DSA_WRUSER (OBJ_NAME(IPT:LENGTH))
         CALL DSA_WRUSER (') does not exist.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOOBJ
         GO TO 500
      END IF
C
C     Now, get its size.  Note we use our own array, which will be big
C     enough (DTA_ arrays are limited to 10 dimensions), just so we can
C     generate a better error message than just have DSA_MAIN_SIZE fail
C     because MAX_DIM is too small.
C
      CALL DSA_MAIN_SIZE(REF_SLOT,.FALSE.,10,NDIM,DARRAY,ERROR,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DSA_WRUSER(
     :       'Failed to get the dimensions of the main data array in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME_UC,STRUCTURE_NAME,IGNORE)
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         CALL DSA_WRUSER('. ')
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR))//'.')
         CALL DSA_WRFLUSH
         GO TO 500
      END IF
C
C     See if the array's dimensions are not too large
C
      IF (NDIM.GT.MAX_DIM) THEN
         IGNORE=0
         CALL DSA_WRUSER('The data array in ')
         CALL DSA_GET_ACTUAL_NAME(REF_NAME_UC,STRUCTURE_NAME,IGNORE)
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         IF (NDIM.EQ.10) THEN
            CALL DSA_WRUSER(' is 10')
         ELSE
            CALL DSA_WRUSER(' is '//ICH_CI(NDIM))
         END IF
         CALL DSA_WRUSER('-dimensional, which is too many dimensions ')
         CALL DSA_WRUSER('for this application to handle.')
         CALL DSA_WRFLUSH
         STATUS=DSA__OVRDIM
         GO TO 500
      END IF
C
C     Calculate number of elements, and return dimensions in user's array.
C
      ELEMENTS=1
      DO I=1,NDIM
         DIMS(I)=DARRAY(I)
         ELEMENTS=ELEMENTS*DIMS(I)
      END DO
      DO I=NDIM+1,MAX_DIM
         DIMS(I)=1
      END DO
C
C     Exit
C
  500 CONTINUE
C
      END
