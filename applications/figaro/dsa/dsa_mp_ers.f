C+
C                          D S A _ M A P _ E R R O R S
C
C  Routine name:
C     DSA_MAP_ERRORS
C
C  Function:
C     Maps the error data array in a structure.
C
C  Description:
C     This routine maps the error data array in a structure, returning
C     the address of the mapped array.  The whole array is mapped.  If
C     there is in fact no error data array, then an array of zeros is
C     generated and its address is returned, unless the mapping is for
C     write or update, in which case an array of zeros is created in
C     the data structure and mapped.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MAP_ERRORS (REF_NAME,MODE,TYPE,ADDRESS,SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) MODE         (Fixed string,descr) One of 'READ','WRITE', or
C                      'UPDATE', indicating the way the data is going to
C                      be accessed.  Only the first character is significant.
C     (>) TYPE         (Fixed string,descr) The type of data array to be
C                      mapped onto the structure array.  This can be 'BYTE',
C                      'CHAR','FLOAT','DOUBLE','SHORT','USHORT' or 'INT'.
C                      If type conversion is needed, it will be performed
C                      automatically.
C     (<) ADDRESS      (Integer,ref) The address of the mapped array.
C     (<) SLOT         (Integer,ref) A handle value associated with this
C                      mapping call, which may be used later to unmap
C                      the data explicitly.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ routines.
C
C  External subroutines / functions used:
C     ICH_FOLD, ICH_CI, ICH_LEN, DSA_FIND_REF, DSA_SEEK_ERRORS,
C     DSA_DATA_SIZE, DSA_MAP_ARRAY, DSA_MAP_DUMMY, DSA_NFILL_ARRAY
C     DSA_GET_ACTUAL_NAME, DSA__ERROR_NAME, DSA__CREATE_ARRAY,
C     DSA_VARIANCE_TO_ERR, DSA_GET_WORK_ARRAY
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 26th October 1994
C-
C  Common variable details:
C     (>) MAX_AXES  (Integer parameter) Maximum number of axes in data.
C     (>) ERROR_UPDATE (Logical array) Indicates that the error array has
C                   been updated (or at least, mapped for update).
C     (<) MAP_CALL_VSLOT(Integer array) Work entry used for err<->variance
C                       array conversions.
C     (>) VARIANCE_CODE (Integer parameter) Error type is `variance'
C
C  Subroutine / function details:
C     ICH_CI        Returns a formatted integer.
C     ICH_FOLD      Convert string to upper case.
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_MAP_ARRAY Map named data array.
C     DSA_DATA_SIZE Get the size of a structure's main data array.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_MAP_DUMMY Map a dummy data array.
C     DSA_GET_ACTUAL_NAME  Get name of structure from ref name.
C     DSA_ZFILL_ARRAY Fill a dummy array with zeros.
C     DSA_SEEK_ERRORS See if an error data array exists.
C     DSA_VARIANCE_TO_ERR  Converts a variance array to an error array.
C     DSA_GET_WORK_ARRAY   Get workspace of specified size and type.
C     DSA__ERROR_NAME Get name of error data object in a structure.
C     DSA__CREATE_ARRAY Creates an array of a given type and size.
C
C  History:
C     8th  Jul 1987  Original version.  KS / AAO.
C     20th Jul 1988  Call to DSA_MAP_ARRAY modified.
C     8th  Sep 1989  Call to DSA_MAP_ARRAY modified - propagation flag
C                    set false.
C     15th Dec 1989  Now uses DSA__ERROR_NAME to get name of array.  KS/AAO.
C     17th Jan 1990  Change to calling sequence for DSA__ERROR_NAME. KS/AAO.
C     12th Mar 1990  Now allows for the possibility that the error array is
C                    a variance array, not an uncertainty array.  KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     6th  Oct 1992  HME / UoE, Starlink.  Zero-initialise the array
C                    in case of write access, both in the data structure
C                    and in the work space.
C     26th Oct 1994  Now uses new calling sequence for DSA_MAP_ARRAY. KS/AAO.
C+
      SUBROUTINE DSA_MAP_ERRORS (REF_NAME,MODE,TYPE,ADDRESS,SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, MODE, TYPE
      INTEGER ADDRESS, SLOT, STATUS
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
      CHARACTER ICH_CI*16
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER CHR*1                       ! First character of MODE
      INTEGER   DIMS(MAX_AXES)              ! Dimensions of data array
      INTEGER   DTA_STATUS                  ! Status code from DTA_ routines
      CHARACTER ERROR*64                    ! DTA_ error description
      INTEGER   ERRORS                      ! Number of conversion errors
      INTEGER   ERR_TYPE                    ! Code for error type - ignored
      LOGICAL   EXIST                       ! True if error data array exists
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   INVOKE                      ! Dummy function return value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*128                    ! Name of dummy or error array
      INTEGER   NDIM                        ! Number of data array dimensions
      INTEGER   NELM                        ! Number of data array elements
      CHARACTER NUMBER*16                   ! Used to format numbers
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*128               ! Name of structure
      CHARACTER TYPE_UC*8                   ! Upper case version of TYPE
      INTEGER   WORK_ADDRESS                ! Address of work array used
      INTEGER   WORK_SLOT                   ! Slot number of work array used
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     We need an upper case version of REF_NAME and TYPE
C
      TYPE_UC=TYPE
      INVOKE=ICH_FOLD(TYPE_UC)
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
C
C     First find the dimensions of the main data array.  Those of
C     the error array should match.
C
      CALL DSA_DATA_SIZE (REF_NAME,MAX_AXES,NDIM,DIMS,NELM,STATUS)
C
C     See if there in in fact any error data.
C
      CALL DSA_SEEK_ERRORS (REF_NAME,EXIST,STATUS)
C
C     See if the mode requires that the array exist
C
      CHR=MODE(1:1)
      INVOKE=ICH_FOLD(CHR)
C
      IF (EXIST.OR.(CHR.EQ.'W').OR.(CHR.EQ.'U')) THEN
C
C        The error array either is required to exist or does exist.
C        Either way, we want to map an actual data object, not a dummy
C        array.
C
C        Look up the reference name in the tables and get the name
C        of the main structure object, then get the name of the error array.
C
         CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
         CALL DSA__ERROR_NAME (REF_SLOT,OBJ_NAME,LENGTH,ERR_TYPE)
C
C        If the array did not exist, we need to create one of the same
C        dimensions as the data array.
C
         IF (.NOT.EXIST) THEN
            CALL DSA__CREATE_ARRAY (OBJ_NAME(:LENGTH),NDIM,
     :                                            DIMS,TYPE,DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               CALL DTA_ERROR (DTA_STATUS,ERROR)
               CALL DSA_WRUSER('Error trying to create error array in ')
               IGNORE=0
               CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
               CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
               CALL DSA_WRUSER('. ')
               CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER('.')
               CALL DSA_WRFLUSH
               DTA_CODE=DTA_STATUS
               STATUS=DSA__DTAERR
               GO TO 500             ! Error exit
            END IF
         END IF
C
C        Map the error array.
C
         CALL DSA_MAP_ARRAY (OBJ_NAME(:LENGTH),MODE,TYPE_UC,
     :             1,NELM,NELM,.FALSE.,.FALSE.,.FALSE.,ADDRESS,
     :                                                SLOT,STATUS)
C
C        If we just created the array, fill it with zeros.
C        Also if access is WRITE, fill the new array with zeros.
C
         IF ((.NOT.EXIST).OR.CHR.EQ.'W') THEN
            CALL DSA_ZFILL_ARRAY (NELM,ADDRESS,TYPE_UC,STATUS)
         END IF
C
C        Now, allow for the possibility that the array we just mapped
C        was actually a variance array.
C
         IF (ERR_TYPE.EQ.VARIANCE_CODE) THEN
C
C           It was, so we need to get a workspace array to serve as the actual
C           error array, and need to convert the data into it.  First get the
C           work array.
C
            CALL DSA_GET_WORK_ARRAY (NELM,TYPE,WORK_ADDRESS,
     :                                                WORK_SLOT,STATUS)
            IF (STATUS.NE.0) GO TO 500      ! Error exit
C
C           Now we flag the map call slot to indicate that it represents
C           the mapping of a variance array tied to an error work array.
C           Not that the use of the -ve slot # indicates the way round the
C           conversion has been done.
C
            MAP_CALL_VSLOT(SLOT)=-WORK_SLOT
C
C           And now we have to process the variance array into an error
C           array. For write mapping or if the array did not exist the
C           variances are all zero, so are the errors. Otherwise we have
C           to convert.
C
            IF (CHR.EQ.'W'.OR.(.NOT.EXIST)) THEN
               CALL DSA_ZFILL_ARRAY (NELM,WORK_ADDRESS,TYPE_UC,STATUS)
            ELSE
               CALL DSA_VARIANCE_TO_ERR (NELM,TYPE,ADDRESS,
     :                                         WORK_ADDRESS,ERRORS)
               IF (ERRORS.NE.0) THEN
                  CALL DSA_WRUSER ('Note: There were ')
                  NUMBER=ICH_CI(ERRORS)
                  CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
                  CALL DSA_WRUSER (' numeric error(s) '//
     :                           'converting the variance array in ')
                  IGNORE=0
                  CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,
     :                                                        IGNORE)
                  CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
                  CALL DSA_WRUSER (' into an error array.')
                  CALL DSA_WRFLUSH
               END IF
            END IF
            ADDRESS=WORK_ADDRESS
         END IF
C
      ELSE
C
C        There is no error array, so we need to generate a dummy one.
C        and fill it with zeros.
C
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,STATUS)
         NAME='error array in '//STRUCTURE
         CALL DSA_MAP_DUMMY (NAME,MODE,TYPE_UC,NELM,ADDRESS,SLOT,STATUS)
C
         CALL DSA_ZFILL_ARRAY (NELM,ADDRESS,TYPE_UC,STATUS)
C
      END IF
C
C     If the mapping was not readonly, set the update flag.
C
      IF (CHR.NE.'R') ERROR_UPDATE(REF_SLOT)=.TRUE.
C
C     Exit
C
  500 CONTINUE
C
      END
