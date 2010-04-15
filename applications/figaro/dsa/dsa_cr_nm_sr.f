C+
C          D S A _ C R E A T E _ N A M E D _ S T R U C T U R E
C
C  Routine name:
C     DSA_CREATE_NAMED_STRUCTURE
C
C  Function:
C     Creates a Figaro output structure, giving it a reference name.
C
C  Description:
C     DSA_CREATE_NAMED_STRUCTURE takes the name of a data structure file
C     and creates in that file a structure as specified by a structure
C     definition file (as read by DSA_READ_STRUCT_DEF) and opens it for
C     output, associating it with a specified reference name.   Note that
C     at least in this implementation, the output structure specified
C     can only be a file name and not a structure within that file, since
C     it is unclear what else should be at the upper levels.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CREATE_NAMED_STRUCTURE (REF_NAME,STRUCTURE_NAME,
C                                                  STRUCTURE_ID,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name to be
C                        associated with the opened data object.
C     (>) STRUCTURE_NAME (Fixed string, descr) The name of the data object
C                        structure to be created - this has to be just
C                        a file name.
C     (>) STRUCTURE_ID   (Fixed string,descr) The name of a structure type
C                        as already defined by a structure definition file.
C     (!) STATUS         (Integer,ref) Status code.  If a bad status value
C                        is passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA_ routines.
C
C  External subroutines / functions used:
C     DTA_ERROR, DTA_ASFNAM, ICH_FOLD, ICH_LEN, DSA_WRUSER, DSA_FNAME,
C     DSA_WRNAME, DSA_INIT_REF_SLOT, DSA__SET_FILE_TYPE, DSA_REAL_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system, and
C     the structure definition must have already been read by DSA_READ_
C     STRUCT_DEF and any necessary structure variables must have been set.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 28th June 1993
C-
C  Subroutine / function details:
C     DTA_ASFNAM    Open a file and associate a top-level name with it
C     DTA_ERROR     Get error text for a DTA_ system error code
C     DSA_DEF_STRUCT_TYPE  Look up structure definition in common & get type
C     DSA_BUILD_STRUCTURE  Create a structure from a structure definition
C     DSA_FNAME     Parse object name into full filename and structure part
C     DSA_INIT_REF_SLOT  Initialise reference slot values
C     DSA__SET_FILE_TYPE Initialise file type variables in common.
C     DSA_REAL_NAME Return real file name corresponding to a temporary name
C     DSA_WRNAME    Output the name of a structure to the user
C     DSA_WRUSER    Output string to user
C     ICH_FOLD      Convert string to upper case.
C     ICH_LEN       Position of last non-blank char in a string
C
C  External variable details:
C     (<) DTA_CODE    (Integer) Last DTA_ system failure status code.
C     (>) MAX_FILES   (Integer parameter) Maximum number of file entries.
C     (<) FILE_COUNT  (Integer array) Number of reference names associated
C                     with each file.
C     (!) FILE_NAMES  (String array) Full file specification for each file.
C     (<) FILE_TOP_NAMES (String array) Top-level name associated with
C                     each file.
C     (!) FILE_USED   (Logical array) Indicates file table slot in use..
C     (>) MAX_REFS    (Integer parameter) Maximum number of reference names.
C     (<) OBJ_LEN     (Integer array) Number of chars in each OBJ_NAME.
C     (<) OBJ_NAMES   (String array) Name (as recognised by DTA_) of data
C                     object corresponding to reference name.
C     (<) ACTUAL_NAMES(String array) Full ref name structure specification.
C     (<) PARM_VALUE  (String array) Parameter value associated with ref name.
C     (<) REF_FILE    (Integer array) File slot number of file in question.
C     (!) REF_NAMES   (String array) Reference names in use.
C     (!) REF_USED    (Logical array) Indicates reference slot in use.
C
C  History:
C     31st August 1987   Original version.  KS / AAO.
C     9th  Sept   1988   PARM_VALUE initialised, together with other
C                        common variables. KS / AAO.
C     28th Nov    1988   Now uses DSA_INIT_REF_SLOT for initialisation. KS/AAO.
C     15th Jan    1990   Now sets the file extension common variables, and
C                        passes REF_SLOT to DSA_BUILD_STRUCTURE.  KS/AAO.
C     19th Jan    1990   DSA_SET_FILE_TYPE now a DSA__ routine.  KS/AAO.
C     15th Feb    1991   Call sequence for DSA_FNAME modified. KS/AAO.
C     21st Aug    1992   Automatic portability modifications
C                        ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug    1992   Remove unused variable declarations. KS/AAO
C     29th Aug 1992      "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_CREATE_NAMED_STRUCTURE (REF_NAME,STRUCTURE_NAME,
     :                                           STRUCTURE_ID,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, STRUCTURE_NAME, STRUCTURE_ID
      INTEGER STATUS
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER ACTUAL_NAME*128                ! Actual full structure name
      INTEGER   DTA_STATUS                     ! Status code from DTA_ calls
      CHARACTER ERROR*64                       ! DTA_ error description
      CHARACTER FILENAME*128                   ! Full file name spec
      INTEGER   FILE_SLOT                      ! Slot # for file name
      LOGICAL   FOUND                          ! Indicates name match in tables
      LOGICAL   FREE                           ! Indicates free slot in tables
      INTEGER   I                              ! Loop variable
      INTEGER   INVOKE                         ! Dummy function reference
      LOGICAL   NEWFILE                        ! Flags new file to be created
      CHARACTER REALNAME*128                   ! Full real file name spec
      CHARACTER REF_NAME_UC*64                 ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                       ! Slot # for reference name
      CHARACTER STRUCT*64                      ! Lower level part of object name
      INTEGER   STRUCT_SLOT                    ! Common slot common definition
      CHARACTER TOP_NAME*64                    ! DTA_ top level name for object
      CHARACTER TYPE*32                        ! Type of structure
      CHARACTER TSTR*80                        ! Local string storage
C
C     Check for bad passed status
C
      IF (STATUS.NE.0) RETURN
C
C     We need to work with an upper case version of REF_NAME
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
C
C     Make sure there's a slot for the reference name - also make
C     sure that it isn't already being used.
C
      FREE=.FALSE.
      FOUND=.FALSE.
      DO I=1,MAX_REFS
         IF (REF_USED(I)) THEN
            IF (REF_NAMES(I).EQ.REF_NAME_UC) THEN
               FOUND=.TRUE.
               GO TO 320      ! Break out of search loop
            END IF
         ELSE
            REF_SLOT=I
            FREE=.TRUE.
         END IF
      END DO
  320 CONTINUE
      IF (FOUND) THEN
         CALL DSA_WRUSER('Attempt to open two data objects with the ')
         CALL DSA_WRUSER('same reference name ('//
     :                     REF_NAME_UC(:ICH_LEN(REF_NAME_UC))//'). ')
         CALL DSA_WRUSER('Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__DUPREF
         GO TO 500                                  ! Error exit
      END IF
      IF (.NOT.FREE) THEN
         CALL DSA_WRUSER(
     :        'Unable to find a spare reference name slot for the ')
         CALL DSA_WRUSER('new name "')
         CALL DSA_WRUSER(REF_NAME_UC(:ICH_LEN(REF_NAME_UC)))
         CALL DSA_WRUSER(
     :      '".  Too many objects open.  Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOREFSL
         GO TO 500                                  ! Error exit
      END IF
C
C     Determine the file name, the extension, and any structure details.
C     Notice that if structure details are specified, we can't handle them.
C     Setting NEWFILE true will force the generation of a new file name
C     - ie one we KNOW doesn't exist and so won't be in the list of files
C     already open.
C
      NEWFILE=.TRUE.
      CALL DSA_FNAME (STRUCTURE_NAME,NEWFILE,' ',FILENAME,STRUCT,STATUS)
      IF (STRUCT.NE.' ') THEN
         CALL DSA_WRUSER('Cannot build a new structure in anything '//
     :      'other than an empty file.  It is not clear what should '//
     :      'go in the upper levels of ')
         CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__BLDERR
         GO TO 500            ! Error exit
      END IF
C
C     Pass through the list of files already open, looking for
C     a spare slot.
C
      FREE=.FALSE.
      DO I=1,MAX_FILES
         IF (.NOT.FILE_USED(I)) THEN
            FREE=.TRUE.
            FILE_SLOT=I
            GO TO 340                 ! Break out of loop
         END IF
      END DO
  340 CONTINUE
      IF (.NOT.FREE) THEN
         TSTR='No slot available for "'//
     :         STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME))//'" '
         CALL DSA_WRUSER(TSTR)
         TSTR='(file '//FILENAME(:ICH_LEN(FILENAME))//'). '
         CALL DSA_WRUSER(TSTR)
         CALL DSA_WRUSER ('There must be too many files open.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOPARSL
         GO TO 500                                  ! Error exit
      END IF
C
C     Fill up the slots, to flag the file as open.  Note, we normally
C     wouldn't do this until after we've succeded in opening the file,
C     but DSA_DEF_STRUCT_TYPE has to be called prior to the opening,
C     and for it to work properly it has to know what type of file is
C     being opened, and that means the common slots must have been
C     filled and DSA__SET_FILE_TYPE called.  So we do it in this order.
C     If the opening fails, we flag the slots as unused again. We allow
C     for the file name being a temporary, and set ACTUAL_NAME in common
C     to reflect the real name.
C
      CALL DSA_REAL_NAME(FILENAME,REALNAME)
      CALL DSA_ACTNAME(REALNAME,STRUCT,ACTUAL_NAME,STATUS)
      TOP_NAME=REF_NAME_UC
      FILE_NAMES(FILE_SLOT)=FILENAME
      FILE_TOP_NAMES(FILE_SLOT)=TOP_NAME
      FILE_USED(FILE_SLOT)=.TRUE.
      FILE_COUNT(FILE_SLOT)=1
      CALL DSA_INIT_REF_SLOT (REF_SLOT,STATUS)
      REF_FILE(REF_SLOT)=FILE_SLOT
      REF_NAMES(REF_SLOT)=REF_NAME_UC
      OBJ_NAMES(REF_SLOT)=TOP_NAME(:ICH_LEN(TOP_NAME))//STRUCT
      OBJ_LEN(REF_SLOT)=ICH_LEN(OBJ_NAMES(REF_SLOT))
      ACTUAL_NAMES(REF_SLOT)=ACTUAL_NAME
      CALL DSA__SET_FILE_TYPE (REF_SLOT,STATUS)
C
C     See if a structure of the type specified by STRUCTURE_ID is
C     known, and if it is, get its type - this is needed for the
C     call to DTA_ASFNAM.
C
      CALL DSA_DEF_STRUCT_TYPE (REF_SLOT,STRUCTURE_ID,.TRUE.,TYPE,
     :                                          STRUCT_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500       ! Error exit
C
C     Try to create the file.
C
      CALL DTA_ASFNAM (REF_NAME_UC,FILENAME,'NEW',0,TYPE,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         DTA_CODE=DTA_STATUS
         CALL DTA_ERROR(DTA_STATUS,ERROR)
         CALL DSA_WRUSER('Error attempting to open output file ')
         CALL DSA_WRUSER(FILENAME(:ICH_LEN(FILENAME)))
         CALL DSA_WRUSER('. ')
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__DTAERR
         REF_USED(REF_SLOT)=.FALSE.
         FILE_USED(FILE_SLOT)=.FALSE.
         GO TO 500                                  ! Error exit
      END IF
C
C     Now, build the structure in the file
C
      CALL DSA_BUILD_STRUCTURE (REF_NAME_UC,REF_SLOT,STRUCT_SLOT,
     :                                                        STATUS)
C
C     Exit
C
  500 CONTINUE
C
      END
