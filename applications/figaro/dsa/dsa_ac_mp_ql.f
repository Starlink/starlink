C+
C                   D S A _ A C T _ M A P _ Q U A L I T Y
C
C  Routine name:
C     DSA_ACT_MAP_QUALITY
C
C  Function:
C     Actually performs the mapping of the quality data array in a structure.
C
C  Description:
C     This routine is called by DSA_MAP_QUALITY and does almost all the
C     actual work involved. The reason for the two routines is that this
C     routine can also be called from DSA_PRE_PROCESS_FLAGGED_VALUES and
C     if we let that routine call DSA_MAP_QUALITY directly then we could
C     no longer assume from the fact that DSA_MAP_QUALITY had been called
C     that the application wanted to handle quality. Splitting the
C     routines in this way allows us to make this assumption.
C     This routine maps the quality data array in a structure, returning
C     the address of the mapped array.  The whole array is mapped.  If
C     there is in fact no quality data array, then one will be created
C     if the mapping specified 'WRITE' or 'UPDATE' and filled with zeros.
C     If the main data array contains flagged values, and the application
C     has not indicated through a call to DSA_USE_FLAGGED_VALUES that it
C     wants to handle these directly, then any flagged values in the
C     data array will be removed and the corresponding elements of the
C     quality array will be set.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ACT_MAP_QUALITY (REF_NAME,MODE,TYPE,ADDRESS,SLOT,STATUS)
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
C     CNF_PVAL, ICH_FOLD, DSA_REF_SLOT, DSA_SEEK_QUALITY,
C     DSA_DATA_SIZE, DSA_MAP_ARRAY, DSA_MAP_DUMMY, DSA_NFILL_ARRAY
C     DSA_GET_ACTUAL_NAME, DSA_SEEK_FLAGGED_VALUES, DSA__QUAL_NAME
C     DSA__CREATE_QUAL_ENV, DSA_CREATE_ARRAY, DSA__GET_BADBITS,
C     DSA__CHECK_BADBITS
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 30th November 1995
C-
C  Common variable details:
C     (>) MAX_AXES     (Integer parameter) Maximum number of axes in data.
C     (<) QUALITY_SLOT (Integer array) Map call slot used for quality mapping.
C     (>) DATA_SLOT    (Integer array) Map call slot used for data mapping.
C     (>) USE_QUALITY  (Logical array) Indicates quality data will be used.
C     (<) QUAL_UPDATE  (Logical array) Indicates that the quality array
C                      has been updated (or at least, mapped for update).
C     (<) QUAL_EXIST   (Integer array) State of knowledge about quality array.
C                      Indicates unknown (0), known not to exist (-1),
C                      exists (1).
C
C  Subroutine / function details:
C     CNF_PVAL          Full pointer to dynamically allocated memory
C     ICH_FOLD          Convert string to upper case.
C     DSA_MAP_ARRAY     Map named data array.
C     DSA_CREATE_ARRAY  Create a named data array.
C     DSA_DATA_SIZE     Get the size of a structure's main data array.
C     DSA_MAP_DUMMY     Map a dummy data array.
C     DSA_REF_SLOT      Get reference slot number for named structure.
C     DSA_GET_ACTUAL_NAME  Get name of structure from ref name.
C     DSA_ZFILL_ARRAY   Fill a dummy array with zeros.
C     DSA_SEEK_QUALITY  See if an quality array exists.
C     DSA_SEEK_FLAGGED_VALUES See if main data array has flagged values.
C     DSA_CHECK_BADBITS Check to see if quality values may be misinterpreted.
C     DSA__CREATE_QUAL_ENV Ensure environment for a quality array exists.
C     DSA__QUAL_NAME    Get name of quality array in structure.
C     DSA__GET_BADBITS  Get value of BADBITS mask for the quality array.
C
C  History:
C     22nd July 1988.   Original version.  KS / AAO.
C     8th  Sept 1989.   Call to DSA_MAP_ARRAY now sets propagation flag
C                       false.  KS/AAO.
C     11th Dec  1989.   Now sets QUAL_UPDATE flag on a write or update
C                       mapping.  KS/AAO.
C     21st Feb  1990.   Uses DSA__QUAL_NAME to remove assumptions about
C                       file format details.  KS/AAO.
C     22nd Apr  1991.   Now uses DSA__CREATE_QUAL_ENV to create the
C                       environment for the quality array - allows support
C                       of structured arrays including BADBITS values. KS/AAO.
C     1st  May  1991.   Now gets BADBITS value and tests to see if this
C                       is likely to cause problems. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992     Remove unused variable declarations. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     16th Jun 1993     Now sets 'quality exist' flag if array created. KS/AAO,
C     26th Oct 1994     Now uses new calling sequence for DSA_MAP_ARRAY. KS/AAO.
C      3rd Feb 1995     Now supports files with both flagged data values and
C                       quality arrays. KS/AAO.
C      7th Feb 1995     DSA_MAP_QUALITY now renamed to DSA_ACT_MAP_QUALITY
C                       and replaced by a new routine that calls this one.
C                       REF_SLOT used in call instead of REF_NAME. KS / AAO.
C     30th Nov 1995.    Added possibility that a copy of the array may be
C                       needed. KS/AAO.
C     2005 June 3       Replace DYNAMIC_MEMORY with
C                       %VAL(CNF_PVAL(ADDRESS)) contruct for 64-bit
C                       addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_ACT_MAP_QUALITY (REF_NAME,MODE,TYPE,ADDRESS,
     :                                                    SLOT,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, MODE, TYPE
      INTEGER ADDRESS, SLOT, STATUS
C
C     Functions used
C
      INTEGER ICH_FOLD
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
      BYTE      BADBITS          ! Badbits quality mask value
      CHARACTER CHR*1            ! First character of MODE
      LOGICAL   COPY             ! True if a copy array needed
      LOGICAL   CREATE_DUMMY     ! True if dummy array needed
      INTEGER   DIMS(MAX_AXES)   ! Dimensions of data array
      LOGICAL   EXIST            ! True if quality data array exists
      LOGICAL   FLAGGED          ! True if data has flagged values
      INTEGER   IGNORE           ! Dummy status argument
      INTEGER   INVOKE           ! Dummy function return value
      INTEGER   LENGTH           ! Object name length
      CHARACTER NAME*128         ! Name of dummy or quality array
      INTEGER   NDIM             ! Number of data array dimensions
      INTEGER   NELM             ! Number of data array elements
      CHARACTER OBJ_NAME*128     ! DTA_ name of data object
      INTEGER   REF_SLOT         ! Ref slot for structure
      CHARACTER STRUCTURE*128    ! Name of structure
      CHARACTER TYPE_UC*8        ! Upper case version of TYPE
      LOGICAL   ZERO_FILL        ! True if array to be zero-filled
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     We need an upper case version of TYPE
C
      TYPE_UC = TYPE
      INVOKE = ICH_FOLD(TYPE_UC)
C
C     Look up the reference name in the tables and get the name
C     of the quality array.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      CALL DSA__QUAL_NAME (REF_SLOT,OBJ_NAME,LENGTH)
C
C     First find the dimensions of the main data array.  Those of
C     the quality array should match.
C
      CALL DSA_DATA_SIZE (REF_NAME,MAX_AXES,NDIM,DIMS,NELM,STATUS)
C
C     See if there is in fact any quality data, and see if the
C     main data array contains flagged values.
C
      CALL DSA_SEEK_QUALITY (REF_NAME,EXIST,STATUS)
      CALL DSA_SEEK_FLAGGED_VALUES (REF_NAME,FLAGGED,STATUS)
      IF (STATUS.NE.0) GO TO 500    ! Error exit
C
C     Initially, assume we want neither to create the array, not to
C     fill it with zeros.
C
      ZERO_FILL = .FALSE.
      CREATE_DUMMY = .FALSE.
C
C     If DSA_MAP_QUALITY is being called directly from the application,
C     then a call that specifies 'WRITE' or 'UPDATE' will expect to see
C     a real array in the final structure and this should be respected,
C     even if the actual data is flagged. However, this routine may be
C     called from DSA_PRE_PROCESS_FLAGGED, in which case we really do
C     want to create a dummy array if one does not exist. We can tell the
C     difference by looking at the 'use quality' flag, which will be set
C     if we are being called from DSA_MAP_QUALITY.
C
      IF ((.NOT.USE_QUALITY(REF_SLOT)).AND.(.NOT.EXIST)) THEN
         CREATE_DUMMY = .TRUE.
         ZERO_FILL = .TRUE.
      ELSE
C
C        Otherwise, see if the mode requires that the array exist
C
         CHR = MODE(1:1)
         INVOKE = ICH_FOLD(CHR)
C
         IF (EXIST.OR.(CHR.EQ.'W').OR.(CHR.EQ.'U')) THEN
C
C           The quality array either is required to exist or does exist.
C           Either way, we want to map an actual data object, not a dummy
C           array.
C
            IF (.NOT.EXIST) THEN
C
C              If the array did not exist, we need to create one of the
C              same dimensions as the data array.
C
               CALL DSA__CREATE_QUAL_ENV (REF_SLOT,STATUS)
               CALL DSA_CREATE_ARRAY (' ',OBJ_NAME(:LENGTH),TYPE_UC,
     :                                              NDIM,DIMS,STATUS)
               IF (STATUS.NE.0) GO TO 500    ! Error exit
               QUAL_EXIST(REF_SLOT) = 1
            END IF
C
C           There is one case where we need to make a copy of the quality
C           array rather than use the actual one in the file. This is where
C           the quality array exists, but the data is flagged and the
C           application will not use the flagged values, intending to use
C           the quality array instead. If all of this is the case, the
C           quality array will be modified to reflect the flagged data -
C           but if the quality array is being mapped readonly, then we need
C           a copy of it! So this is an odd combination of circumstances,
C           but one that has to be allowed for.
C
            COPY = (EXIST.AND.(CHR.EQ.'R').AND.FLAGGED.AND.
     :            USE_QUALITY(REF_SLOT).AND..NOT.USE_FLAGS(REF_SLOT))
C
C           Map the quality array.
C
            CALL DSA_MAP_ARRAY (OBJ_NAME(:LENGTH),MODE,TYPE_UC,1,NELM,
     :                NELM,COPY,.FALSE.,.FALSE.,ADDRESS,SLOT,STATUS)
C
C           If we just created the array, we will want to fill it with zeros.
C           If it existed already, we should check to see if it has values
C           that should be masked by the BADBITS mask but will be mis-
C           interpreted by DSA as it stands at present.
C
            IF (EXIST) THEN
               CALL DSA__GET_BADBITS (REF_SLOT,BADBITS,IGNORE)
               CALL DSA_CHECK_BADBITS (REF_NAME,%VAL(CNF_PVAL(ADDRESS)),
     :                                    NELM,TYPE_UC,BADBITS,STATUS)
            ELSE
               ZERO_FILL = .TRUE.
            END IF
         ELSE
C
C           There is no quality array, so we need to generate a dummy one.
C           and will want to fill it with zeros.
C
            CREATE_DUMMY = .TRUE.
            ZERO_FILL = .TRUE.
         END IF
      END IF
C
C     Now, after all that, we will either have mapped an array in the
C     structure starting at address ADDRESS (which we may have had to
C     create) or we may have CREATE_DUMMY set.  Either way, we may
C     also have ZERO_FILL set.
C
      IF (CREATE_DUMMY) THEN
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,STATUS)
         NAME = 'quality array in '//STRUCTURE
         CALL DSA_MAP_DUMMY (NAME,MODE,TYPE_UC,NELM,ADDRESS,SLOT,STATUS)
      END IF
      IF (ZERO_FILL) THEN
         CALL DSA_ZFILL_ARRAY (NELM,ADDRESS,TYPE_UC,STATUS)
      END IF
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     If the mapping was not readonly, set the update flag.
C
      IF (CHR.NE.'R') QUAL_UPDATE(REF_SLOT) = .TRUE.
C
C     Tie in the quality mapping to the reference name in the common tables
C
      QUALITY_SLOT(REF_SLOT) = SLOT
C
C     If the data array has already been mapped, then now is the time to
C     call the quality pre-processing routine.  Note that we can't assume
C     from the obvious fact that DSA_ACT_MAP_QUALITY has being called that
C     the application wants to handle quality data.  This is because
C     DSA_PRE_PROCESS_FLAGGED_VALUES may call DSA_MAP_QUALITY if a
C     quality array actually exists. But we can use the USE_QUALITY flag
C     to tell the difference.
C
      IF (USE_QUALITY(REF_SLOT).AND.(DATA_SLOT(REF_SLOT).NE.0)) THEN
         CALL DSA_PRE_PROCESS_QUALITY (REF_NAME,STATUS)
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
