C+
C                       D S A _ N A M E D _ O U T P U T
C
C  Routine name:
C     DSA_NAMED_OUTPUT
C
C  Function:
C     Opens a Figaro structure for output, giving it a reference name.
C
C  Description:
C     DSA_NAMED_OUTPUT takes the name of a data structure object -
C     either just a file name, eg DISK:[DIR]FILE, or a structured name
C     combining a file name with the name of a structure within that
C     file, eg DISK$DATA:[DIR.SUBDIR]FILE.DATA_STRUCTURE - and opens it
C     for output, associating it with a specified reference name.  If
C     the structure object is already known to the DSA system, having
C     been opened already, then that already open structure will be
C     used, unless the call explicitly specifies that a new structure
C     is to be created.  If the structure object is not already open, a
C     new one will be created, based on another - already opened -
C     whose reference name is supplied.  If no basis name is supplied
C     (ie is passed as blank), then an empty data structure will be
C     created.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_NAMED_OUTPUT (REF_NAME, STRUCTURE_NAME, BASIS_NAME,
C                                  BASIS_FLAGS, DETAIL_FLAGS, STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name to be
C                        associated with the opened data object.
C     (>) STRUCTURE_NAME (Fixed string, descr) The name of the input
C                        data object, as described above.
C     (>) BASIS_NAME     (Fixed string,descr) The reference name of the
C                        already opened data object to serve as the basis
C                        for the output.  This may be blank, in which
C                        case an empty struicture will be created.
C     (>) BASIS_FLAGS    (Integer,ref) Flags that control the use made of
C                        the basis object.  Those used at present are:
C                        bit 0:  If set, data and axis structures are not to
C                        be copied. This usually implies that the new
C                        structure will have differently dimensioned data
C                        arrays which will be created by routines such as
C                        DSA_RESHAPE_AXIS, DSA_RESHAPE_DATA.
C     (>) DETAIL_FLAGS   (Integer,ref) Flags that control other details of
C                        the structure opening.  Those used at present are:
C                        bit 0: If set, creation of a new data structure is
C                        forced, even if one of the same name is already
C                        known to the system.
C                        Note that bit 0 is the least significant bit.
C     (!) STATUS         (Integer,ref) Status code.  If a bad status value
C                        is passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA_ routines.
C
C  External subroutines / functions used:
C     DTA_ERROR, DTA_ASFNAM, DTA_CYVAR, DTA_TYVAR, ICH_FOLD, ICH_LEN,
C     DSA_WRUSER, DSA_FNAME, DSA_WRNAME, DTA_NMVAR, DSA_INIT_REF_SLOT,
C     DSA__SET_FILE_TYPE, DSA__SAME_FILE_FORMAT, DSA__TOP_ITEM_TYPE
C     DSA_REAL_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 28th June 1993
C-
C  Subroutine / function details:
C     DTA_ASFNAM    Open a file and associate a top-level name with it
C     DTA_ERROR     Get error text for a DTA_ system error code
C     DTA_TYVAR     Get the type of a data object
C     DTA_CYVAR     Copy a data structure object
C     DTA_NMVAR     Get name of element in data structure
C     DSA_FNAME     Parse object name into full filename and structure part
C     DSA_INIT_REF_SLOT  Set initial values in reference table entries
C     DSA__SET_FILE_TYPE Set file type variables in common
C     DSA__SAME_FILE_FORMAT Set file type to match that of another file
C     DSA__TOP_ITEM_TYPE Classify structure item (data, axis, etc)
C     DSA_REAL_NAME Get real name corresponding to a temporary file name
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
C     (<) REF_FILE    (Integer array) File slot number of file in question.
C     (!) REF_NAMES   (String array) Reference names in use.
C     (!) REF_USED    (Logical array) Indicates reference slot in use.
C
C  History:
C     14th July 1987   Original version.  KS / AAO.
C     21st July 1988   Additional common items initialised.  KS / AAO.
C     9th  Sept 1988   PARM_VALUE initialised.  KS / AAO.
C     28th Nov  1988   DSA_INIT_REF_SLOT now used for initialisation KS/AAO.
C     14th Feb  1989   Comments revised. KS/AAO.
C     15th Jan  1990   Call to DSA_SET_FILE_TYPE added.  KS/AAO.
C     19th Jan  1990   DSA_SET_FILE_TYPE is now DSA__etc.  Use of new routine
C                      DSA__SAME_FILE_FORMAT added.  KS/AAO.
C     15th Feb  1990   Call was still to DSA_SET_FILE_TYPE, and not DSA__etc!
C                      Call corrected.  KS/AAO.
C     26th Feb  1990   Explicit knowledge of data structure item names (.X,
C                      .Y etc) removed.  Now uses DSA__ routines rather than
C                      assuming original Figaro format.  KS/AAO.
C     12th Mar  1990   Parameters to DSA__TOP_ITEM_TYPE revised.  KS/AAO.
C     15th Feb  1991   Now forces file extension to match that for the
C                      basis file type. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     4th  Sep 1992    Changed extensions to lowercase. HME/UoE.
C     28th Jun 1993    Added use of DSA_REAL_NAME so error messages using
C                      the file actual name in common reflected the real name
C                      of the file. KS/AAO.
C+
      SUBROUTINE DSA_NAMED_OUTPUT (REF_NAME,STRUCTURE_NAME,BASIS_NAME,
     :                                 BASIS_FLAGS,DETAIL_FLAGS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, STRUCTURE_NAME, BASIS_NAME
      INTEGER BASIS_FLAGS, DETAIL_FLAGS, STATUS
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
      LOGICAL   AXIS_TYPE                      ! Indicates axis data item
      CHARACTER BASIS_NAME_UC*64               ! Upper case BASIS_NAME
      INTEGER   BASIS_SLOT                     ! Ref slot for BASIS_NAME
      LOGICAL   COPY_DATA                      ! Flags data is to be copied
      LOGICAL   DATA_TYPE                      ! Indicates data array item
      CHARACTER DEFEXT*4                       ! File extension for new file
      INTEGER   DTA_STATUS                     ! Status code from DTA_ calls
      CHARACTER ERROR*64                       ! DTA_ error description
      LOGICAL   EXIST                          ! Output structure exists
      CHARACTER FILENAME*128                   ! Full file name spec
      INTEGER   FILE_SLOT                      ! Slot # for file name
      LOGICAL   FILE_OPEN                      ! Indicates file was opened
      LOGICAL   FOUND                          ! Indicates name match in tables
      LOGICAL   FREE                           ! Indicates free slot in tables
      CHARACTER FROM*80                        ! Data object being copied
      INTEGER   I                              ! Loop variable through tables
      INTEGER   INVOKE                         ! Dummy function reference
      INTEGER   IPOS                           ! Number of sub structure
      CHARACTER NAME*16                        ! Name of structure item
      LOGICAL   NEWFILE                        ! Flags new file to be created
      INTEGER   NM_STATUS                      ! Status from DTA_NMVAR
      CHARACTER REALNAME*128                   ! Full real file name spec
      CHARACTER REF_NAME_UC*64                 ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                       ! Slot # for reference name
      CHARACTER STRUCT*64                      ! Lower level part of object name
      CHARACTER TO*80                          ! Data object being created
      CHARACTER STRING*80                      ! Local string storage
      CHARACTER TOP_NAME*64                    ! DTA_ top level name for object
      CHARACTER TYPE*16                        ! Object type - ignored
C
C     Check for bad passed status
C
      IF (STATUS.NE.0) RETURN
C
C     We need to work with an upper case version of REF_NAME and BASIS_NAME
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      BASIS_NAME_UC=BASIS_NAME
      INVOKE=ICH_FOLD(BASIS_NAME_UC)
C
C     Make sure there's a slot for the reference name - also make
C     sure that it isn't already being used, and that the basis name
C     is known to us.
C
      FILE_OPEN=.FALSE.
      FREE=.FALSE.
      BASIS_SLOT=0
      FOUND=.FALSE.
      DO I=1,MAX_REFS
         IF (REF_USED(I)) THEN
            IF (REF_NAMES(I).EQ.REF_NAME_UC) THEN
               FOUND=.TRUE.
               IF (BASIS_SLOT.NE.0) GO TO 320          ! Break out of loop.
            ELSE IF (REF_NAMES(I).EQ.BASIS_NAME_UC) THEN
               BASIS_SLOT=I
               IF (FOUND) GO TO 320      ! Break out of loop if both found.
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
      IF ((BASIS_SLOT.EQ.0).AND.(BASIS_NAME.NE.' ')) THEN
         CALL DSA_WRUSER('Unable to use the reference name ')
         CALL DSA_WRUSER(BASIS_NAME_UC(:ICH_LEN(BASIS_NAME_UC)))
         CALL DSA_WRUSER('as a basis for the new reference name ')
         CALL DSA_WRUSER(REF_NAME_UC(:ICH_LEN(REF_NAME_UC)))
         CALL DSA_WRUSER(
     :    ', as it has not been opened.  Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__BSNOTOP
         GO TO 500                                  ! Error exit
      END IF
C
C     Look at the flags.  (This rather crude code is OK so long
C     as each one should only be 1 or zero.)
C
      IF (BASIS_FLAGS.EQ.0) THEN
         COPY_DATA=.TRUE.
      ELSE IF (BASIS_FLAGS.EQ.1) THEN
         COPY_DATA=.FALSE.
      ELSE
         CALL DSA_WRUSER(
     :        'Warning: unused flags are set in "BASIS_FLAGS" ')
         CALL DSA_WRUSER('in a call to open an output structure. ')
         CALL DSA_WRUSER(
     :       'It may be that an old DSA_ version is being used.')
         CALL DSA_WRFLUSH
         COPY_DATA=MOD(BASIS_FLAGS,2).EQ.0
      END IF
      IF (DETAIL_FLAGS.EQ.0) THEN
         NEWFILE=.FALSE.
      ELSE IF (DETAIL_FLAGS.EQ.1) THEN
         NEWFILE=.TRUE.
      ELSE
         CALL DSA_WRUSER(
     :        'Warning: Unused flags are set in "DETAIL_FLAGS" ')
         CALL DSA_WRUSER('in a call to open an output structure. ')
         CALL DSA_WRUSER(
     :       'It may be that an old DSA_ version is being used.')
         CALL DSA_WRFLUSH
         NEWFILE=MOD(DETAIL_FLAGS,2).NE.0
      END IF
C
C     Determine the file name, the extension, and any structure details
C     If NEWFILE is true, then this will force the generation of a
C     new file name - ie one we KNOW doesn't exist and so won't be
C     in the list of files already open.  Rather messily, we force the
C     default extension to match that for the type of the basis file
C     (note: not its actual extension, but the standard one for its type.)
C     It seems a pity to have introduced the literal file extensions in
C     this code and it would be better to parse the basis file name and
C     use its actual extension.
C
      IF (BASIS_SLOT.EQ.0) THEN
         DEFEXT=' '
      ELSE
         IF (NDF_FORMAT(BASIS_SLOT)) THEN
            DEFEXT='.sdf'
         ELSE
            DEFEXT='.dst'
         END IF
      END IF
      CALL DSA_FNAME (STRUCTURE_NAME,NEWFILE,DEFEXT,FILENAME,
     :                                                STRUCT,STATUS)
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
            IF (NEWFILE) GO TO 340                 ! Break out of loop
         END IF
      END DO
  340 CONTINUE
      IF ((.NOT.FOUND).AND.(.NOT.FREE)) THEN
         STRING='No slot available for "'//
     :              STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME))//'" '
         CALL DSA_WRUSER (STRING)
         STRING='(file '//FILENAME(:ICH_LEN(FILENAME))//'). '
         CALL DSA_WRUSER (STRING)
         CALL DSA_WRUSER ('There must be too many files open.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOPARSL
         GO TO 500                                  ! Error exit
      END IF
C
      IF (.NOT.FOUND) THEN
C
C        Try to create the file.  Note that if we haven't opened the file
C        yet as an input file, we CREATE a new file, even if one of the
C        same name already exists.  So, we may have to make a second call
C        to DSA_FNAME to force it to give us the name of a file that
C        we know does not yet exist.
C
         IF (.NOT.NEWFILE) THEN
            NEWFILE=.TRUE.
            CALL DSA_FNAME (STRUCTURE_NAME,NEWFILE,DEFEXT,FILENAME,
     :                                                  STRUCT,STATUS)
         END IF
         TYPE=' '
         IF (BASIS_SLOT.GT.0) THEN
            CALL DTA_TYVAR (
     :                    OBJ_NAMES(BASIS_SLOT)(:OBJ_LEN(BASIS_SLOT)),
     :                                                TYPE,DTA_STATUS)
         END IF
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
            GO TO 500                                  ! Error exit
         END IF
         FILE_OPEN=.TRUE.
      END IF
C
C     Fill up the slots, to flag the file as open.
C
      CALL DSA_REAL_NAME(FILENAME,REALNAME)
      CALL DSA_ACTNAME(REALNAME,STRUCT,ACTUAL_NAME,STATUS)
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
      REF_USED(REF_SLOT)=.TRUE.
      OBJ_NAMES(REF_SLOT)=TOP_NAME(:ICH_LEN(TOP_NAME))//STRUCT
      OBJ_LEN(REF_SLOT)=ICH_LEN(OBJ_NAMES(REF_SLOT))
      ACTUAL_NAMES(REF_SLOT)=ACTUAL_NAME
      IF (BASIS_SLOT.NE.0) THEN
         CALL DSA__SAME_FILE_FORMAT (REF_SLOT,BASIS_SLOT,STATUS)
      ELSE
         CALL DSA__SET_FILE_TYPE (REF_SLOT,STATUS)
      END IF
C
C     If the object in question is not the top-level name of the file,
C     see if it exists.  If a new file has just been opened, then
C     nothing will exist in it yet.
C
      IF (STRUCT.NE.' ') THEN
         CALL DTA_TYVAR(OBJ_NAMES(REF_SLOT),TYPE,DTA_STATUS)
         EXIST=DTA_STATUS.EQ.0
      ELSE
         EXIST=.NOT.FILE_OPEN
      END IF
C
      IF ((.NOT.EXIST).AND.(BASIS_SLOT.GT.0)) THEN
C
C        It doesn't exist, so we have to create it from the basis
C        structure.
C
         IF (COPY_DATA) THEN
C
C           If we are to copy all of the basis structure, it's
C           just a simple packaged recursive structure copy.
C
            FROM=OBJ_NAMES(BASIS_SLOT)
            TO=OBJ_NAMES(REF_SLOT)
            CALL DTA_CYVAR(FROM,TO,DTA_STATUS)
         ELSE
C
C           If we are to omit the data, then we have to restrict
C           the copy.  We copy all the structures bar the axis and
C           data structures.
C
            IPOS=1
            DTA_STATUS=0
            NM_STATUS=0
            DO WHILE (NM_STATUS.EQ.0)
               CALL DTA_NMVAR(OBJ_NAMES(BASIS_SLOT),IPOS,NAME,
     :                                                 NM_STATUS)
               IF (NM_STATUS.EQ.0) THEN
                  IPOS=IPOS+1
                  CALL DSA__TOP_ITEM_TYPE (BASIS_SLOT,NAME,.TRUE.,
     :                                           AXIS_TYPE,DATA_TYPE)
                  IF ((.NOT.AXIS_TYPE).AND.(.NOT.DATA_TYPE)) THEN
                     FROM=OBJ_NAMES(BASIS_SLOT)(:OBJ_LEN(BASIS_SLOT))
     :                                                    //'.'//NAME
                     TO=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))
     :                                                    //'.'//NAME
                     CALL DTA_CYVAR(FROM,TO,DTA_STATUS)
                     NM_STATUS=DTA_STATUS
                  END IF
               END IF
            END DO
         END IF
C
C        If the copying failed, DTA_STATUS shows it.
C
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_WRUSER('Error while copying  ')
            CALL DSA_WRNAME(FROM)
            CALL DSA_WRUSER(' to ')
            CALL DSA_WRNAME(TO)
            CALL DTA_ERROR(DTA_STATUS,ERROR)
            CALL DSA_WRUSER('. ')
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('.')
            CALL DSA_WRFLUSH
            DTA_CODE=DTA_STATUS
            STATUS=DSA__DTAERR
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
