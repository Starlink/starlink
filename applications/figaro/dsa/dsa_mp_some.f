C+
C                   D S A _ M A P _ S O M E _ D A T A
C
C  Routine name:
C     DSA_MAP_SOME_DATA
C
C  Function:
C     Maps part of the main data array in a structure.
C
C  Description:
C     This routine maps the main data array in a structure, returning
C     the address of the dynamic memory array that may be used to
C     access it.   A subset of the whole array is mapped.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MAP_SOME_DATA (REF_NAME, NINDEX, INDICES, NMAP, MODE, TYPE,
C                                          ADDRESS, SLOT, STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) NINDEX       (Integer, ref) The number of dimensions specified in
C                      the INDICES array. This should be either match the
C                      number of dimensions in the array, or should be one
C                      in which case INDICES is a single integer giving the
C                      number of the first element in the array to be mapped.
C     (>) INDICES      (Integer array, ref) Used to specify the first
C                      element in the array to be mapped. If NINDEX is the
C                      actual number of dimensions in the array then INDICES
C                      contains the index number of the first mapped element
C                      in the various array dimensions. In some case it is
C                      simpler to give just the number (from 1) of the first
C                      element to be mapped, and this can be done if NINDEX
C                      is set to one. If the array is one-dimensional, the
C                      two cases are identical.
C     (>) NMAP         (Integer,ref) The number of elements to be mapped.
C     (>) MODE         (Fixed string,descr) One of 'READ','WRITE', or
C                      'UPDATE', indicating the way the data is going to
C                      be accessed.  Only the first character is significant.
C     (>) TYPE         (Fixed string,descr) The type of data array to be
C                      mapped onto the structure array.  This can be 'BYTE',
C                      'CHAR','FLOAT','DOUBLE','SHORT', 'USHORT' or 'INT'.
C                      If type conversion is needed, it will be performed
C                      automatically.
C     (<) ADDRESS      (Integer,ref) The memory address of the start of
C                      the mapped data array.
C     (<) SLOT         (Integer,ref) A handle value that identifies the
C                      mapping, and can be used, for example, in a
C                      subsequent call to DSA_UNMAP.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ routines.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, DSA_FIND_REF, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     DSA_MAIN_SIZE, ICH_FOLD, DSA_SEEK_FLAGGED_VALUES,
C     DSA_SEEK_QUALITY, DSA_TYPESIZE, DSA__SET_FLAGGED, DSA__ARRAY,
C     DSA__DATA_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 27th October 1994
C-
C  Common variable details:
C     (>) MAX_AXES     (Integer parameter) Maximum number of axes in data.
C     (>) USE_FLAGS    (Logical array) Indicates application can handle flagged
C                      data values.
C     (<) DATA_UPDATE  (Logical array) Indicates that the data array has
C                      been updated (or at least, mapped for update).
C     (>) USE_QUALITY  (Logical array) Indicates application will use a data
C                      quality array.
C     (<) DATA_SLOT    (Integer array) Map call slot used for data mapping.
C
C  Subroutine / function details:
C     ICH_FOLD      Convert string to upper case.
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_MAP_ARRAY Map named data array.
C     DSA_MAIN_SIZE Get the size of a structure's main data array.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_WRUSER    Output a message string to the user.
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C     DSA_SEEK_FLAGGED_VALUES See if data array contains flagged values.
C     DSA_SEEK_QUALITY See if data array has quality data associated.
C     DSA_TYPESIZE   Gets number of bytes in an element of given type.
C     DSA__SET_FLAGGED  Sets `data flagged' flag in structure
C     DSA__ARRAY     Determines if a named object is a (structured) array.
C     DSA__DATA_NAME Gets name of main data array in a structure.
C
C  History:
C     24th Jun 1987  Original version, based on DSA_MAP_DATA.  KS / AAO.
C+
      SUBROUTINE DSA_MAP_SOME_DATA (REF_NAME,NINDEX,INDICES,NMAP,MODE,
     :                                       TYPE,ADDRESS,SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, MODE, TYPE
      INTEGER ADDRESS, NINDEX, INDICES(*), NMAP, SLOT, STATUS
C
C     Functions used
C
      LOGICAL DSA__ARRAY
      INTEGER DSA_TYPESIZE, ICH_FOLD, ICH_LEN
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      LOGICAL   COPY                        ! Force mapped array to be a copy
      INTEGER   DIMS(MAX_AXES)              ! Dimensions of data array
      CHARACTER ERROR*64                    ! DTA_ error description
      LOGICAL   FLAGS_EXIST                 ! Flagged data values present
      LOGICAL   FLAGS_USED                  ! True if flagged values handled
      INTEGER   I                           ! Loop variable
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   INVOKE                      ! Dummy function return value
      INTEGER   LENGTH                      ! Object name length
      INTEGER   MSTART                      ! First array element to be mapped
      INTEGER   NDIM                        ! Number of data array dimensions
      INTEGER   NELM                        ! Number of data array elements
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      LOGICAL   QUALITY_EXIST               ! Quality info present
      LOGICAL   QUALITY_USED                ! True if quality data handled
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE_NAME*128          ! Full structure name from ref_name
      LOGICAL   TOO_HARD                    ! Array too complicated for us
      CHARACTER TYPE_UC*8                   ! Upper case version of TYPE
      LOGICAL   UNFLAG                      ! Flagged values to be processed out
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
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
C     Look up the reference name in the tables and get the data
C     array dimensions.
C
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      CALL DSA_MAIN_SIZE(REF_SLOT,.FALSE.,10,NDIM,DIMS,ERROR,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DSA_WRUSER(
     :          'Unable to get dimensions of main data array in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE_NAME,IGNORE)
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         CALL DSA_WRUSER('. ')
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR))//'.')
         CALL DSA_WRFLUSH
         GO TO 500
      END IF
C
C     Calculate total number of elements in the array.
C
      NELM=1
      DO I=1,NDIM
         NELM=NELM*DIMS(I)
      END DO
C
C     Work out the start element requested by the caller. (Remember that
C     MSTART is a start number, not an offset. This calculation is easier
C     to do with offsets, which is what the INDICES(I)-1 values are.)
C
      MSTART=0
      DO I=NINDEX,2,-1
         MSTART=(MSTART+INDICES(I)-1)*DIMS(I-1)
      END DO
      MSTART=MSTART+INDICES(1)
C
C     The whole process is complicated by the question of data quality,
C     and the need to pre-process (and post-process) the mapped arrays
C     in order to handle flagged pixel values that will not be handled
C     properly by the calling routine. For the moment, in this early
C     version of the code, we take the easy way out. If there is any
C     need for fancy handling of the data, we simply fall back on mapping
C     the whole array using DSA_MAP_DATA. If things are simple, however,
C     we can and do handle part of the array.  first we need to find out
C     the state of things.
C
      CALL DSA_SEEK_FLAGGED_VALUES (REF_NAME,FLAGS_EXIST,STATUS)
      CALL DSA_SEEK_QUALITY (REF_NAME,QUALITY_EXIST,STATUS)
      IF (QUALITY_EXIST.AND.FLAGS_EXIST) THEN
         CALL DSA_WRUSER('The structure ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE_NAME,IGNORE)
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         CALL DSA_WRUSER(' is inconsistent.  It has both flagged data')
         CALL DSA_WRUSER(' values and a data quality array.  It is')
         CALL DSA_WRUSER(' impossible to know which one to use.')
         CALL DSA_WRFLUSH
         STATUS=DSA__INCSTR
         GO TO 500
      END IF
      FLAGS_USED = USE_FLAGS(REF_SLOT)
      QUALITY_USED = USE_QUALITY(REF_SLOT)
C
C     The cases we can't handle are where quality information exists in
C     the file but the caller wants to handle it using flagged values (in
C     which case we need to map the quality array and flag the relevant
C     values), or where the actual data is flagged but the caller isn't
C     able to handle flagged values (in which case they need to be removed
C     and either presented to the caller as a quality array or just saved
C     for later re-flagging). These cases we leave to DSA_MAP_DATA.
C
      TOO_HARD=.FALSE.
      IF (QUALITY_EXIST.AND.FLAGS_USED) TOO_HARD=.TRUE.
      IF (FLAGS_EXIST.AND.(.NOT.FLAGS_USED)) TOO_HARD=.TRUE.
C
C     If we can't handle it, use DSA_MAP_DATA.
C
      IF (TOO_HARD) THEN
C
C        DSA_MAP_DATA maps the whole of the array, so we have to correct
C        the address it returns (which is the address of the start of the
C        array) to produce the address of the subset the user asked for.
C
         CALL DSA_MAP_DATA (REF_NAME,MODE,TYPE,ADDRESS,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
         ADDRESS = ADDRESS + DSA_TYPESIZE(TYPE,STATUS)*(MSTART-1)
C
      ELSE
C
C        We can do it ourselves, using a call to DSA_MAP_ARRAY that specifies
C        a limited subset of the data. This code is the same as that used at
C        this point by DSA_MAP_DATA, except for the use of an array subset.
C
C        Map the data array.  Allow for the possibility that the named object
C        was itself the data array (this is the case if it is either primitive
C        or a structured array - such as a scaled array).  If so, use its
C        name directly; otherwise get what should be the name of the actual
C        data array.
C
         IF (.NOT.DSA__ARRAY(OBJ_NAME(:LENGTH))) THEN
            CALL DSA__DATA_NAME (REF_SLOT,OBJ_NAME,LENGTH)
         END IF
         CALL DSA_MAP_ARRAY (OBJ_NAME(:LENGTH),MODE,TYPE_UC,MSTART,NMAP,
     :              NELM,COPY,UNFLAG,FLAGS_EXIST,ADDRESS,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500      ! Error exit
C
C        If this is not a readonly mapping, set the update flag.  Also,
C        clear the range update flag - it is most unlikely that the updated
C        range values will continue to be correct if DSA_SET_RANGE has been
C        called prior to an update mapping call.  Moreover, for an update
C        mapping, if the application is using flagged values and the structure
C        does not have data quality information, make sure the `data flagged'
C        flag for the structure is set. Also set the data slot to give a
C        handle to the data mapping.
C
         IF ((MODE(1:1).NE.'R').AND.(MODE(1:1).NE.'r')) THEN
            DATA_UPDATE(REF_SLOT)=.TRUE.
            RANGE_UPDATE(REF_SLOT)=.FALSE.
            IF (USE_FLAGS(REF_SLOT).AND.
     :                     .NOT.(QUALITY_EXIST.OR.FLAGS_EXIST)) THEN
               CALL DSA__SET_FLAGGED (REF_SLOT,.TRUE.,IGNORE)
            END IF
         END IF
         DATA_SLOT(REF_SLOT)=SLOT
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
