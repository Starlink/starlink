C+
C                       D S A _ N A M E D _ I N P U T
C
C  Routine name:
C     DSA_NAMED_INPUT
C
C  Function:
C     Opens a Figaro structure for input, giving it a reference name.
C
C  Description:
C     DSA_NAMED_INPUT takes the name of a data structure object - either
C     just a file name, eg DISK:[DIR]FILE, or a structured name combining
C     a file name with the name of a structure within that file, eg
C     DISK$DATA:[DIR.SUBDIR]FILE.DATA_STRUCTURE - and opens it for input,
C     associating it with a specified reference name.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_NAMED_INPUT (REF_NAME,STRUCTURE_NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name to be
C                        associated with the opened data object.
C     (>) STRUCTURE_NAME (Fixed string, descr) The name of the input
C                        data object, as described above.
C     (!) STATUS         (Integer,ref) Status code.  If a bad status value
C                        is passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA_ routines.
C
C  External subroutines / functions used:
C     DTA_ERROR, DTA_ASFNAM, DTA_TYVAR, ICH_FOLD, ICH_LEN,
C     DSA_WRUSER, DSA_FNAME, DSA_INIT_REF_SLOT, DSA__SET_FILE_TYPE
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DTA_ASFNAM    Open a file and associate a top-level name with it
C     DTA_ERROR     Get error text for a DTA_ system error code
C     DTA_TYVAR     Get the type of a data object
C     DSA_INIT_REF_SLOT  Initialise reference slot values
C     DSA__SET_FILE_TYPE Set file type common variables for ref slot.
C     DSA_FNAME     Parse object name into full filename and structure part
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
C     (<) REF_FILE    (Integer array) File slot number of file in question.
C     (!) REF_NAMES   (String array) Reference names in use.
C     (!) REF_USED    (Logical array) Indicates reference slot in use.
C
C  History:
C     15th June 1987   Original version.  KS / AAO.
C     21st July 1988   Additional common items initialised.  KS / AAO.
C     9th  Sept 1988   PARM_VALUE initialised.  KS / AAO.
C     28th Nov  1988   Now uses DSA_INIT_REF_SLOT for initialisation. KS/AAO.
C     15th Jan  1990   Call to DSA_SET_FILE_TYPE added.  KS/AAO.
C     19th Jan  1990   DSA_SET_FILE_TYPE is now a DSA__ routine.  KS/AAO.
C     15th Feb  1991   Add new extension parameter to DSA_FNAME call. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     14th Mar 1996    Added _UPDATE entry point. HME / UoE, Starlink.
C+
      SUBROUTINE DSA_NAMED_INPUT (REF_NAME,STRUCTURE_NAME,STATUS)
C
      IMPLICIT NONE
      ENTRY DSA_NAMED_INPUT_UPDATE (REF_NAME,STRUCTURE_NAME,STATUS)
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, STRUCTURE_NAME
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
      LOGICAL   FILE_OPEN                      ! Indicates file was opened
      LOGICAL   FOUND                          ! Indicates name match in tables
      LOGICAL   FREE                           ! Indicates free slot in tables
      INTEGER   I                              ! Loop variable through tables
      INTEGER   INVOKE                         ! Dummy function reference
      LOGICAL   NEW                            ! Used to indicate existing file
      CHARACTER REF_NAME_UC*64                 ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                       ! Slot # for reference name
      CHARACTER STRUCT*64                      ! Lower level part of object name
      CHARACTER STRING*80                      ! Local string storage
      CHARACTER TOP_NAME*64                    ! DTA_ top level name for object
      CHARACTER TYPE*16                        ! Object type - ignored
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
      FILE_OPEN=.FALSE.
      FREE=.FALSE.
      FOUND=.FALSE.
      DO I=1,MAX_REFS
         IF (REF_USED(I)) THEN
            IF (REF_NAMES(I).EQ.REF_NAME_UC) THEN
               FOUND=.TRUE.
               GO TO 320                          ! Break out of loop
            END IF
         ELSE
            REF_SLOT=I
            FREE=.TRUE.
         END IF
      END DO
  320 CONTINUE
      IF (FOUND) THEN
         CALL DSA_WRUSER('Attempt to open two data objects with the ')
         STRING='same reference name ('//
     :           REF_NAME_UC(:ICH_LEN(REF_NAME_UC))//'). '
         CALL DSA_WRUSER(STRING)
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
C     Determine the file name, the extension, and any structure details
C
      NEW=.FALSE.
      CALL DSA_FNAME (STRUCTURE_NAME,NEW,' ',FILENAME,STRUCT,STATUS)
C
C     Pass through the list of files already open, looking for a) the
C     file itself, to see if it is already open, b) a spare slot, in
C     case it isn't.
C
      FOUND=.FALSE.
      FREE=.FALSE.
      DO I=1,MAX_FILES
         IF (FILE_USED(I)) THEN
            IF (FILE_NAMES(I).EQ.FILENAME) THEN
               FOUND=.TRUE.
               FILE_SLOT=I
               GO TO 340                           ! Break out of loop
            END IF
         ELSE
            FREE=.TRUE.
            FILE_SLOT=I
         END IF
      END DO
  340 CONTINUE
      IF ((.NOT.FOUND).AND.(.NOT.FREE)) THEN
         STRING='No slot available for "'//
     :           STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME))//'" '
         CALL DSA_WRUSER (STRING)
         STRING='(file '//FILENAME(:ICH_LEN(FILENAME))//'). '
         CALL DSA_WRUSER (STRING)
         CALL DSA_WRUSER ('There must be too many files open.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOPARSL
         GO TO 500                                  ! Error exit
      END IF
C
C     Try to open the file
C
      IF (.NOT.FOUND) THEN
         CALL DTA_ASFNAM (REF_NAME_UC,FILENAME,'OLD',0,' ',DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            DTA_CODE=DTA_STATUS
            CALL DTA_ERROR(DTA_STATUS,ERROR)
            CALL DSA_WRUSER('Error attempting to open input file ')
            CALL DSA_WRUSER(FILENAME(:ICH_LEN(FILENAME)))
            CALL DSA_WRUSER('. ')
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('.')
            CALL DSA_WRFLUSH
            STATUS=DSA__DTAERR
            GO TO 500                                  ! Error exit
         END IF
         FILE_OPEN=.TRUE.
      END IF
C
C     Fill up the slots, to flag the file as open.
C
      CALL DSA_ACTNAME(FILENAME,STRUCT,ACTUAL_NAME,STATUS)
      IF (.NOT.FOUND) THEN
         TOP_NAME=REF_NAME_UC
         FILE_NAMES(FILE_SLOT)=FILENAME
         FILE_TOP_NAMES(FILE_SLOT)=TOP_NAME
         FILE_USED(FILE_SLOT)=.TRUE.
         FILE_COUNT(FILE_SLOT)=1
      ELSE
         TOP_NAME=FILE_TOP_NAMES(FILE_SLOT)
         FILE_COUNT(FILE_SLOT)=FILE_COUNT(FILE_SLOT)+1
      END IF
      CALL DSA_INIT_REF_SLOT (REF_SLOT,STATUS)
      REF_FILE(REF_SLOT)=FILE_SLOT
      REF_NAMES(REF_SLOT)=REF_NAME_UC
      OBJ_NAMES(REF_SLOT)=TOP_NAME(:ICH_LEN(TOP_NAME))//STRUCT
      OBJ_LEN(REF_SLOT)=ICH_LEN(OBJ_NAMES(REF_SLOT))
      ACTUAL_NAMES(REF_SLOT)=ACTUAL_NAME
      CALL DSA__SET_FILE_TYPE (REF_SLOT,STATUS)
C
C     If the object in question is not the top-level name of the file,
C     have a quick look at it just to make sure it exists.
C
      IF (STRUCT.NE.' ') THEN
         CALL DTA_TYVAR(OBJ_NAMES(REF_SLOT),TYPE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_WRUSER('Error attempting to access input object ')
            CALL DSA_WRUSER(STRUCT(:ICH_LEN(STRUCT)))
            CALL DSA_WRUSER(' in file ')
            CALL DSA_WRUSER(FILENAME(:ICH_LEN(FILENAME)))
            CALL DSA_WRUSER('. ')
            CALL DTA_ERROR(DTA_STATUS,ERROR)
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('. ')
            CALL DSA_WRFLUSH
C
C           Since this file won't do, we have to close it
C
            REF_USED(REF_SLOT)=.FALSE.
            IF (FILE_OPEN) THEN
               CALL DTA_FCLOSE(REF_NAME_UC,DTA_STATUS)
               FILE_USED(FILE_SLOT)=.FALSE.
            ELSE
               FILE_COUNT(FILE_SLOT)=FILE_COUNT(FILE_SLOT)-1
            END IF
            STATUS=DSA__NOOBJ
            GO TO 500                                  ! Error exit
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
