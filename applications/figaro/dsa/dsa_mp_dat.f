C+
C                        D S A _ M A P _ D A T A
C
C  Routine name:
C     DSA_MAP_DATA
C
C  Function:
C     Maps the main data array in a structure.
C
C  Description:
C     This routine maps the main data array in a structure, returning
C     the address of the dynamic memory array that may be used to
C     access it.   The whole array is mapped.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MAP_DATA (REF_NAME,MODE,TYPE,ADDRESS,SLOT,STATUS)
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
C     DSA_MAIN_SIZE, ICH_FOLD, DTA_STRUC, DTA_TYVAR, DSA_SEEK_FLAGGED_VALUES,
C     DSA_SEEK_QUALITY, DSA_PRE_PROCESS_FLAGGED_VALUES,
C     DSA_PRE_PROCESS_QUALITY, DSA__SET_FLAGGED, DSA__ARRAY, DSA__DATA_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 3rd February 1995
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
C     (>) QUALITY_SLOT (Integer array) Map call slot used for quality mapping.
C     (>) MAP_CALL_NFLAG(Integer array) Number of values unflagged in array.
C
C  Subroutine / function details:
C     ICH_FOLD      Convert string to upper case.
C     ICH_LEN       Position of last non-blank char in string.
C     DTA_STRUC     Determine if a DTA_ object is primitive or a structure.
C     DTA_TYVAR     Get the type of a data object.
C     DSA_MAP_ARRAY Map named data array.
C     DSA_MAIN_SIZE Get the size of a structure's main data array.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_WRUSER    Output a message string to the user.
C     DSA_GET_ACTUAL_NAME  Get the full structure name from a ref_name.
C     DSA_SEEK_FLAGGED_VALUES See if data array contains flagged values.
C     DSA_SEEK_QUALITY See if data array has quality data associated.
C     DSA_PRE_PROCESS_QUALITY Perform quality pre-processing.
C     DSA_PRE_PROCESS_FLAGGED_VALUES  Perform flagged value pre-processing.
C     DSA__SET_FLAGGED  Sets `data flagged' flag in structure
C     DSA__ARRAY     Determines if a named object is a (structured) array.
C     DSA__DATA_NAME Gets name of main data array in a structure.
C
C  History:
C     24th Jun 1987  Original version.  KS / AAO.
C     22nd Jul 1988  Data quality/flagged value handling added.  Call
C                    to DSA_MAP_ARRAY modified.  KS/AAO.
C     8th  Sep 1989  Control of flagged value propagation added.  Call
C                    to DSA_MAP_ARRAY modified.  KS /AAO.
C     13th Dec 1989  Now sets `data flagged' flag in structure if
C                    appropriate.  Also now uses DSA__ routines to
C                    handle the details of structure contents. KS/AAO.
C     3rd  May 1990  Now clears the `data flagged' flag in the structure
C                    if it was set but no elements were actually flagged.
C                    KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     8th  Oct 1992  HME / UoE, Starlink.  Unflag the data only if the
C                    mode is not WRITE. Prologue now states correctly
C                    that USE_FLAGS and USE_QUALITY are LOGICAL.
C     26th Oct 1994  Now uses new calling sequence for DSA_MAP_ARRAY. KS/AAO.
C      3rd Feb 1995  Now supports files that have both flagged data and
C                    quality arrays. KS/AAO.
C+
      SUBROUTINE DSA_MAP_DATA (REF_NAME,MODE,TYPE,ADDRESS,SLOT,STATUS)
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
      LOGICAL DSA__ARRAY
      INTEGER ICH_FOLD, ICH_LEN
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
      INTEGER   I                           ! Loop variable
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   INVOKE                      ! Dummy function return value
      INTEGER   LENGTH                      ! Object name length
      INTEGER   NDIM                        ! Number of data array dimensions
      INTEGER   NELM                        ! Number of data array elements
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      LOGICAL   QUALITY_EXIST               ! Quality info present
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE_NAME*128          ! Full structure name from ref_name
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
C     Calculate number of elements.
C
      NELM=1
      DO I=1,NDIM
         NELM=NELM*DIMS(I)
      END DO
C
C     We need to see if we have to ask DSA_MAP_ARRAY to ensure that the
C     array is a copy, and if we have to ask for flagged data to be
C     removed.  It needs to be a copy if a) the data is flagged, but
C     flags will not be used (and so need removing), or b) there is a
C     quality array but flagged data will be used (and so the flags need
C     inserting).  The reason for insisting on a copy in these circumstances
C     is that if a program crashes after fiddling with a data array that is
C     mapped directly to disk and before resetting these fiddles, the disk
C     data will end up in the fiddled state.  Data needs unflagging at
C     this stage only if it contains flags but neither quality nor flags
C     are going to be used by the application.  Important: Note that
C     the DSA_PRE_PROCESS_xxx routines assume that this routine has called
C     both of DSA_SEEK_FLAGGED_VALUES and DSA_SEEK_QUALITY (to set the
C     common table flags).
C
      CALL DSA_SEEK_FLAGGED_VALUES (REF_NAME,FLAGS_EXIST,STATUS)
      CALL DSA_SEEK_QUALITY (REF_NAME,QUALITY_EXIST,STATUS)
      UNFLAG=.FALSE.
      IF (FLAGS_EXIST) THEN
         IF ((.NOT.USE_FLAGS(REF_SLOT)).AND.
     :               (.NOT.USE_QUALITY(REF_SLOT))) UNFLAG=.TRUE.
      END IF
      COPY=.FALSE.
      IF (FLAGS_EXIST.AND.(.NOT.USE_FLAGS(REF_SLOT))) COPY=.TRUE.
      IF (QUALITY_EXIST.AND.USE_FLAGS(REF_SLOT)) COPY=.TRUE.
C
C     We unflag the data only if the mode is not WRITE. Write access data
C     contain garbage after mapping. Checking them against the bad value
C     runs the risk of encountering an "invalid operand" and of setting
C     other IEEE flags like inexact.
C
      IF ((MODE(1:1).EQ.'W').OR.(MODE(1:1).EQ.'w')) UNFLAG=.FALSE.
C
C     Map the data array.  Allow for the possibility that the named object
C     was itself the data array (this is the case if it is either primitive
C     or a structured array - such as a scaled array).  If so, use its
C     name directly; otherwise get what should be the name of the actual
C     data array.
C
      IF (.NOT.DSA__ARRAY(OBJ_NAME(:LENGTH))) THEN
         CALL DSA__DATA_NAME (REF_SLOT,OBJ_NAME,LENGTH)
      END IF
      CALL DSA_MAP_ARRAY (OBJ_NAME(:LENGTH),MODE,TYPE_UC,1,NELM,
     :              NELM,COPY,UNFLAG,FLAGS_EXIST,ADDRESS,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500      ! Error exit
C
C     If we explicitly tried to unflag the data but there were no flagged
C     values, clear the `data flagged' flag - so we don't waste time doing
C     that again. (Having MAP_CALL_NFLAG in common is a kludge - it should
C     really be a parameter to DSA_MAP_ARRAY)
C
      IF (UNFLAG.AND.(MAP_CALL_NFLAG(SLOT).EQ.0)) THEN
         CALL DSA__SET_FLAGGED (REF_SLOT,.FALSE.,IGNORE)
      END IF
C
C     If this is not a readonly mapping, set the update flag.  Also,
C     clear the range update flag - it is most unlikely that the updated
C     range values will continue to be correct if DSA_SET_RANGE has been
C     called prior to an update mapping call.  Moreover, for an update
C     mapping, if the application is using flagged values and the structure
C     does not have data quality information, make sure the `data flagged'
C     flag for the structure is set. Also set the data slot to give a
C     handle to the data mapping.
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
C
C     Finally, see if we need to call either of the pre-process routines.
C     If the application is going to use flagged data, we can call
C     DSA_PRE_PROCESS_FLAGGED_VALUES now.  If it is going to use quality
C     data, then we call DSA_PRE_PROCESS_QUALITY from whichever of this
C     and DSA_MAP_QUALITY is called second (because both need to have
C     been called).
C
      IF (USE_FLAGS(REF_SLOT)) THEN
         CALL DSA_PRE_PROCESS_FLAGGED_VALUES (REF_NAME,STATUS)
      ELSE IF (USE_QUALITY(REF_SLOT).AND.
     :                            (QUALITY_SLOT(REF_SLOT).NE.0)) THEN
         CALL DSA_PRE_PROCESS_QUALITY (REF_NAME,STATUS)
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
