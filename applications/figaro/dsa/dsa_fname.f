C+
C                          D S A _ F N A M E
C
C  Routine name:
C     DSA_FNAME
C
C  Function:
C     Splits an object name into a full filename and a structure part.
C
C  Description:
C     Given the name of a data object, this routine splits it into a
C     file part and a structure part, returning the full filename and
C     the structure name. Figaro allows a data object to be specified as
C     a filename followed optionally by the name of an object that is part
C     of the hierarchical structure held in the file.  For example, on VMS
C     an object name might be '[.SUBDIR]FILE.Z.DATA' where .Z.DATA is the
C     name of the object in the file and '[.SUBDIR]FILE' specifies the name
C     of the file, which would expand into a full filename as returned by
C     this routine such as 'DISK$DATA:[USER.SUBDIR]FILE.DST;1'. Under UNIX
C     a similar file specification might be 'subdir/file.z.data', which
C     would again be split into '.Z.DATA' (names of data objects are not
C     case-sensitive) and a full filename such as '/usr/user/subdir/file.dst'
C     (UNIX file names are case-sensitive). Note that there is some
C     ambiguity here due to the use of a '.' to delimit both file extensions
C     and the names of data objects, but both conventions seem too deep
C     seated to be changed. So there have to be conventions to deal with
C     the case where a file extension needs to be specified explicitly.
C     There is also the question of the possible use of version numbers
C     in file names. Note that this routine is concerned entirely with the
C     parsing of the file and structure specifications; although it may
C     need to check on the existence of files in order to resolve
C     ambiguities in the parsing, it does not concern itself with the
C     question of whether or not the system will actually be able to open
C     the file whose name it returns.
C
C     This routine calls a system-dependent routine, DSAZ_FNAME, to do
C     most of the actual work for it, since most of the file-handling
C     that is required involves machine-specific code that may not
C     even compile on other machines. There are versions of DSAZ_FNAME
C     for all the supported systems, and the conventions each uses for
C     delaing with explicit extensions and version numbers are to be
C     found in the comments for those system-dependent routines.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_FNAME (OBJECT,NEW,DEFEXT,FILENAME,STRUCT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) OBJECT       (Fixed string,descr) The name of the data object.
C     (>) NEW          (Logical)  True if it is intended that a new file
C                      will eventually be created.
C     (>) DEFEXT       (Fixed string,descr) If NEW is true, then DEFEXT
C                      can be used to force the use of a specific file
C                      extension.  This should be the extension to be used
C                      (unless overidden explicitly in OBJECT) and should
C                      be prefaced by '.'.  It can be left blank, in which
C                      case the default extension will be used for new files.
C                      DEFEXT is ignored if NEW is false.
C     (<) FILENAME     (Fixed string,descr) The full file specification.
C                      If the underlying operating system is not case
C                      sensitive, FILENAME will be folded to upper case,
C                      otherwise it will be as supplied in OBJECT.
C     (<) STRUCT       (Fixed string,descr) The structure part of the name,
C                      including the initial '.'  STRUCT will be in upper case.
C     (!) STATUS       (Integer,ref) Status code.  If bad status (non-zero) is
C                      passed to it this routine will return immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_DEFEXT, DSA_WRUSER, DSA_WRFLUSH, DSAZ_FNAME, GEN_EXIST, ICH_LEN
C     ICH_CI
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 17th Dec 1992.
C-
C  Subroutine / function details:
C     DSA_DEFEXTS    Returns default file extensions.
C     DSA_WRUSER     Output string to user.
C     DSA_WRFLUSH    Flush buffered output to user.
C     DSAZ_FNAME     System-dependent object name parsing.
C     GEN_EXIST      Indicates if a named file exists
C     ICH_CI         Formats an integer into a character string.
C     ICH_LEN        Return position of last non-blank char in string.
C
C  Common variables used:
C     (>) MAX_TEMP_NAMES (Integer parameter) Maximum number of temporary
C                        file names.
C     (<) FILE_TEMP_NAMES(String array) Temporary names used for files.
C     (<) FILE_REAL_NAMES(String array) Name file really ought to have.
C     (!) NAME_USED      (Logical array) Flags slot entry in use.
C
C  History:
C     12th June 1987   Original version.  KS / AAO.
C     8th  Sept 1989   Now accepts the syntax with the ';' delimiting
C                      the file name and extension, without needing
C                      the parentheses.  KS/AAO.
C     14th Feb  1991   Now allows for more than one possible file extension,
C                      ie can look for a '.DST' file if no '.SDF' files is
C                      found, depending on what DSA_DEFEXTS returns. KS/AAO
C     15th Feb  1991   DEFEXT parameter added. KS/AAO.
C     13th Mar  1991   Outputs a warning if there is more than one possible
C                      file extension and files with both extensions exist
C                      already. KS/AAO.
C     21st Aug  1992   Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     28th Aug  1992   Completely re-worked to make use of DSAZ_ routines
C                      to produce a portable version. KS/AAO.
C     29th Aug  1992   "INCLUDE" filenames now upper case. Format of
C                      ambiguity message modified. KS/AAO.
C     31st Aug  1992   Now makes better use of message returned by DSAZ_FNAME
C                      when an error occurs. KS/AAO.
C     17th Dec  1992   Added creation of temporary file name, if necessary.
C                      KS/AAO.
C+
      SUBROUTINE DSA_FNAME (OBJECT,NEW,DEFEXT,FILENAME,STRUCT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL NEW
      CHARACTER*(*) OBJECT, DEFEXT, FILENAME, STRUCT
      INTEGER STATUS
C
C     Functions used
C
      LOGICAL   GEN_EXIST
      INTEGER   ICH_LEN
      CHARACTER ICH_CI*10
C
C     DSA common variables
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      CHARACTER ALTFILE*256                ! Name of file with alternative .ext
      CHARACTER CHR                        ! Character from filename
      CHARACTER DEFEXTS(2)*16              ! Default file extensions
      CHARACTER DIRNAME*256                ! Directory specification
      LOGICAL   EXISTS                     ! True if named file exists
      INTEGER   I                          ! Loop index through name slots
      INTEGER   ICH                        ! Loop index through filename
      INTEGER   INAME                      ! Number used to make temp file name
      INTEGER   LENAME                     ! Number of chars in FILENAME
      INTEGER   LROOT                      ! Charactrs in DIRNAME
      INTEGER   NAME_SLOT                  ! Temp file table free slot number
      INTEGER   NEXTS                      ! Number of possible extensions
C
      IF (STATUS.NE.0) RETURN
C
C     Get the possible default extensions used by data files. If a new
C     file is to be created, then any extension specified in DEFEXT will
C     override these, and only one default extension will be needed.
C
      IF (NEW) THEN
         IF (DEFEXT.NE.' ') THEN
            DEFEXTS(1)=DEFEXT
         ELSE
            CALL DSA_DEFEXTS(1,DEFEXTS,NEXTS)
         END IF
         NEXTS=1
      ELSE
         CALL DSA_DEFEXTS(2,DEFEXTS,NEXTS)
      END IF
C
C     Use the system-dependent routine DSAZ_FNAME to do most of the
C     work.  Note that this routine does not report any errors, so
C     we have to do that for it. About the only error it can find is
C     a parsing error - most real errors will be found when the system
C     attempts to open the file.
C
      CALL DSAZ_FNAME (OBJECT,NEW,DEFEXTS,NEXTS,FILENAME,STRUCT,
     :                                              ALTFILE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DSA_WRUSER('Cannot make sense of the name "')
         CALL DSA_WRUSER(OBJECT(:ICH_LEN(OBJECT)))
         CALL DSA_WRUSER('". '//ALTFILE(:ICH_LEN(ALTFILE)))
         CALL DSA_WRUSER('. This is not a valid specification for'//
     :                         ' a Figaro data object.')
         CALL DSA_WRFLUSH
      ELSE
C
C        If there was ambiguity in the filename, usually because of the
C        possibility of differing default extensions, report that.
C
         IF (ALTFILE.NE.' ') THEN
            CALL DSA_WRUSER('Note: The specification "')
            CALL DSA_WRUSER(OBJECT(:ICH_LEN(OBJECT)))
            CALL DSA_WRUSER('" could specify either of the files:')
            CALL DSA_WRFLUSH
            CALL DSA_WRUSER(FILENAME(:ICH_LEN(FILENAME)))
            CALL DSA_WRUSER('     or')
            CALL DSA_WRFLUSH
            CALL DSA_WRUSER(ALTFILE(:ICH_LEN(ALTFILE)))
            CALL DSA_WRFLUSH
            CALL DSA_WRUSER('The first of these is assumed to be ')
            CALL DSA_WRUSER(' the correct file to use.')
            CALL DSA_WRFLUSH
         END IF
      END IF
C
C     There is another check we have to perform. If NEW is set, we guarantee
C     that we will return the name of a file that does not exist. However,
C     under some systems, DSAZ_FNAME will not be able to make that guarantee
C     of the file name it produces (under UNIX, for example, if file version
C     numbers are not being used). In that case, we need to generate a temporary
C     name for the file, which we have to record in common for renaming later.
C
      IF (NEW) THEN
         IF (GEN_EXIST(FILENAME(:ICH_LEN(FILENAME)))) THEN
C
C           It does exist, we do need a temporary name. First find a common
C           slot in which to record it. (MAX_TEMP_NAMES is as large as
C           MAX_FILES, so we know there will be an available slot).
C
            DO I=1,MAX_TEMP_NAMES
               IF (.NOT.NAME_USED(I)) THEN
                  NAME_SLOT=I
                  GO TO 340     ! Break from name search loop
               END IF
            END DO
  340       CONTINUE
            FILE_REAL_NAMES(NAME_SLOT)=FILENAME
            NAME_USED(NAME_SLOT)=.TRUE.
C
C           Now work out a suitable temporary name. We try to keep it in the
C           same directory as the original file - looking for ']' or '/'
C           to delimit the file name is a bit crude - ideally a system-
C           dependent routine would do this for us.
C
            DO ICH=LEN(FILENAME),1,-1
               CHR=FILENAME(ICH:ICH)
               IF ((CHR.EQ.']').OR.(CHR.EQ.'/')) THEN
                  DIRNAME=FILENAME(:ICH)
                  LROOT=ICH
                  GO TO 360
               END IF
            END DO
            LROOT=0
  360       CONTINUE
            INAME=1
            EXISTS=.TRUE.
            DO WHILE (EXISTS)
               IF (LROOT.GT.0) THEN
                  FILENAME=DIRNAME(:LROOT)//'Temp_File_'//
     :                                              ICH_CI(INAME)
               ELSE
                  FILENAME='Temp_File_'//ICH_CI(INAME)
               END IF
               LENAME=ICH_LEN(FILENAME)
               FILENAME(LENAME+1:)='.tmp'
               LENAME=LENAME+4
               EXISTS=GEN_EXIST(FILENAME(:LENAME))
               INAME=INAME+1
            END DO
            FILE_TEMP_NAMES(NAME_SLOT)=FILENAME
         END IF
      END IF
C
      END
