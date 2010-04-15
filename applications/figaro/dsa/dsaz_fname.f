C+
C                      D S A Z _ F N A M E
C
C  Routine name:
C     DSAZ_FNAME
C
C  Function:
C     Splits an object name into a full filename and a structure part.
C
C  Description:
C     This is the system-dependent part of the routine DSA_FNAME, which
C     takes an object name and splits it up into a full unambiguous
C     filename and, if specified, the hierarchical name of a data object
C     held in that file.  This involves an intimate knowledge of the
C     file name syntax of the machine in question, and ambiguities may
C     need to be resolved by looking at the existence of actual files
C     which may also be a system-dependent operation.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSAZ_FNAME (OBJECT,NEW,DEFEXTS,NEXTS,FILENAME,STRUCT,
C                                                      ALTFILE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) OBJECT       (Chcracter*(*)) The name of the data object.
C     (>) NEW          (Logical)  True if it is intended that a new file
C                      will eventually be created.
C     (>) DEFEXTS      (Character(NEXTS)*(*)) The possible default
C                      file extensions, in order of priority. Each array
C                      element should be a character string giving the
C                      extension preceded by a '.'. The case of DEFEXTS
C                      is left unchanged by this routine, and whether it
C                      is respected or not depends on the Operating system.
C     (>) NEXTS        (Integer) The number of possible default extensions.
C     (<) FILENAME     (Character*(*)) The full file specification.
C     (<) STRUCT       (Character*(*)) The structure part of the name,
C                      including the initial '.' STRUCT will be returned
C                      in upper case.
C     (<) ALTFILE      (Character*(*)) If there is any ambiguity in the
C                      name of the data file, an alternate file name is
C                      returned in ALTFILE. (This is intended to be used only
C                      to generate a warning message). If STATUS is returned
C                      indicating an error, ALTFILE is used to return a more
C                      informative error message.
C     (!) STATUS       (Integer) Status code.  If a bad status value
C                      (non-zero) is passed to it, this routine will return
C                      immediately.
C
C  External variables used:  None.
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 31st August 1992
C-
C  System:
C     This is the UNIX version of DSAZ_FNAME.
C
C  Syntax accepted:
C     The basic form of OBJECT is a string of the form 'file.struct'
C     where 'file' is a UNIX filename and 'struct' is the hierarchical name
C     of a structure object within the file. The file specification may be
C     absolute or relative, and may contain the names of subdirectories
C     if these are needed or even '..' type specifications. So any of the
C     following are acceptable forms of specifying an object. Note that
C     none of these specifications contain a file extension - the default
C     Figaro extension will be assumed for these:
C
C     '/home/aaossg/ks/file.data_array'
C     '../dir/subdir/file.x.data'
C     'file'   (no object within the file is specified here)
C
C     Any string prefaced by a '$' in the object name is taken to be
C     an environment variable and is translated. So if 'mydata' is an
C     environment variable set to '/data/ks' then the following are also
C     valid specifications:
C
C     '$mydata/file.x.data'
C     '$mydata/../user/file.data_array'
C
C     (This syntax would normally be converted in the same way by a shell,
C     but not all Figaro file names pass through the shell - some may be
C     read directly by the parameter system prompting the user.)
C
C     If more than one default extension is allowed, a new file will be
C     assumed to take the first default extension, and the extension
C     assumed for an existing file will depend on which files, if any,
C     actually exist. So if '.sdf' and .'dst' are both possible extensions,
C     then an old file called 'file' will be taken to be whichever of
C     'file.ndf' and 'file.dst' actually exists. If both exist, the ambiguity
C     will be flagged. Note that the accepted default extensions are
C     controlled by the environment variable FIGARO_FORMATS.
C
C     The specification 'file.dst.x.data' is ambiguous, since it might
C     refer to '.dst.x.data' within a file 'file', or, -more likely- to
C     '.x.data' within the file 'file.dst'. If the first component of
C     what looks like a structure name matches one of the allowed default
C     extensions, then it is assumed to be an explicit extension specification.
C     If an extension that is not one of the Figaro defaults is to be
C     specified, then it has to be placed in parentheses, eg 'file(.myext)'
C     in order to remove any ambiguity.
C
C     If version numbers are being supported, then these are accepted as
C     well, and 'file.dst,23.x.data' refers to the object '.x.data' in the
C     file 'file.dst,23'.  The presence of the comma makes it quite clear
C     that the '.dst' here is an extension, and in this case even an
C     unexpected extension need not be placed in parentheses.
C
C  External subroutines / functions used:
C     DSA_DEFEXTS, DTA_VERSNAME, EMS_ANNUL, EMS_BEGIN, EMS_END,
C     ICH_DELIM, ICH_FOLD, ICH_LEN, GEN_GETCWD, PSX_GETENV
C
C  Subroutine / function details:
C     DSA_DEFEXTS    Returns default file extensions.
C     DTA_VERSNAME   Apply version numbers to file name (shouldn't really
C                    be using a DTA routine here!)
C     EMS_ANNUL      Clear any current EMS errors.
C     EMS_BEGIN      Set new EMS error reporting environment
C     EMS_END        End EMS error reporting environment
C     ICH_DELIM      Position of next of a set of specified delimiters
C     ICH_FOLD       Convert string to upper case.
C     ICH_LEN        Return position of last non-blank char in string.
C     GEN_GETCWD     Get current default directory.
C     PSX_GETENV     Translate environment variable.
C
C  History:
C     28th Aug 1992  Original version. KS/AAO.
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     31st Aug 1992  Environment variable syntax changed to match that
C                    used by a UNIX shell. KS/AAO.
C     19th Oct 1992  HME / UoE, Starlink.  Changed DTAZ_VERSNAME to
C                    DTA_*, is a dummy routine anyway.
C     28th Jul 1993  HME / UoE, Starlink.  Changed PSX_GETCWD to GEN_*.
C     14th Jan 1994  HME / UoE, Starlink.  Fix small typo in prologue.
C      1st Jul 2004  AA / Exeter, Starlink. Non-platform specific version
C+
      SUBROUTINE DSAZ_FNAME (OBJECT,NEW,DEFEXTS,NEXTS,FILENAME,STRUCT,
     :                                                  ALTFILE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL NEW
      INTEGER NEXTS, STATUS
      CHARACTER*(*) OBJECT, DEFEXTS(*), FILENAME, STRUCT, ALTFILE
C
C     Functions used
C
      INTEGER   ICH_DELIM, ICH_FOLD, ICH_LEN
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER CHR             ! Character in string
      INTEGER   EMSTAT          ! Status used for EMS calls
      CHARACTER ERROR*128       ! Used to format error string
      LOGICAL   EXIST1          ! True if file named by FILE1 exists
      LOGICAL   EXIST2          ! True if file named by FILE2 exists
      LOGICAL   EXPEXT          ! True if explicit extension given in OBJECT
      CHARACTER EXT*64          ! Extension specified in OBJECT
      CHARACTER FILE1*256       ! First possible file name (+version #)
      CHARACTER FILE2*256       ! Second possible file name (+version #)
      INTEGER   FLEN            ! Number of characters in FULL_NAME
      CHARACTER FULL_NAME*256   ! Full name of file
      INTEGER   I               ! Loop index through characters in string
      INTEGER   ICOMMA          ! Position of comma in structure name
      INTEGER   IDELIM          ! Position of environment variable delimiter
      INTEGER   IDOLLR          ! Position of next '$' in string
      INTEGER   IDOT            ! Position of last '.' in string
      INTEGER   IDOTS           ! Position of '../' in string
      INTEGER   IEXEND          ! End of file extension in structure name
      INTEGER   IEXT            ! Index through default extensions
      INTEGER   ILBRK           ! Position of '(' in string
      LOGICAL   INBRK           ! True if within parentheses
      INTEGER   INVOKE          ! Used to invoke a function
      INTEGER   IRBRK           ! Position of ')' in string
      INTEGER   ISLASH          ! Position of last '/' in string
      INTEGER   ISLEN           ! Length of STRUCT_NAME
      INTEGER   ISTAT           ! Status used by INQUIRE statements
      INTEGER   LENEXT          ! Length of default extension
      CHARACTER OBJECT_NAME*256 ! OBJECT with environment variables translated
      INTEGER   PSLASH          ! Position of penultimate '/' in string
      CHARACTER STRUCT_NAME*128 ! Structure name specified in OBJECT
      INTEGER   TLEN            ! Number of characters in TRAN_NAME
      CHARACTER TRAN_NAME*256   ! Translated value of environment variable
C
      IF (STATUS.NE.0) RETURN
C
C     Initialise.
C
      INBRK=.FALSE.
C
C     Since we have PSX calls, we set up a new EMS environment to suppress
C     any error messages from them.
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Pass through OBJECT first looking for and attempting to translate
C     any environment variables (anything starting with '$').
C
      ERROR=' '
      OBJECT_NAME=OBJECT
      IDOLLR=INDEX(OBJECT_NAME,'$')
      DO WHILE(IDOLLR.GT.0)
         IDELIM=ICH_DELIM(OBJECT_NAME,IDOLLR+1,'/(.$ ')
         IF (IDELIM.EQ.0) IDELIM=LEN(OBJECT_NAME)+1
         CALL PSX_GETENV(OBJECT_NAME(IDOLLR+1:IDELIM-1),TRAN_NAME,
     :                                                        STATUS)
         IF (STATUS.NE.0) THEN
            ERROR='Unable to translate environment variable "'//
     :                            OBJECT_NAME(IDOLLR+1:IDELIM-1)//'"'
            STATUS=DSA__OBJINV
            GO TO 500      ! Error exit
         END IF
         TLEN=ICH_LEN(TRAN_NAME)
         IF (TLEN.LE.0) THEN
            ERROR='Invalid translation of environment variable "'//
     :                            OBJECT_NAME(IDOLLR+1:IDELIM-1)//'"'
            STATUS=DSA__OBJINV
            GO TO 500      ! Error exit
         END IF
         OBJECT_NAME(IDOLLR:)=TRAN_NAME(:TLEN)//OBJECT_NAME(IDELIM:)
         IDOLLR=INDEX(OBJECT_NAME,'$')
      END DO
C
C     Parse the filename. We start by assuming none of the various
C     components are present, then pass through the string trying to
C     identify them.  The VAX version used a table-driven parser for this,
C     but the UNIX syntax is rather easier to deal with and a less subtle
C     aproach should be OK.
C
      ILBRK=0
      IRBRK=0
      IDOT=0
      ISLASH=0
      EXPEXT=.FALSE.
      DO I=1,LEN(OBJECT_NAME)
         CHR=OBJECT_NAME(I:I)
         IF (CHR.EQ.' ') GO TO 340   ! Break out of parse loop
         IF (CHR.EQ.'/') THEN
            ISLASH=I
            IDOT=0
         ELSE IF (CHR.EQ.'(') THEN
            INBRK=.TRUE.
            ILBRK=I
         ELSE IF (CHR.EQ.')') THEN
            INBRK=.FALSE.
            IRBRK=I
         ELSE IF (CHR.EQ.'.') THEN
            IF (.NOT.INBRK) THEN
               IF (IDOT.EQ.0) IDOT=I
            END IF
         END IF
      END DO
  340 CONTINUE
C
C     Some basic tests to see if those make sense.
C
      IF ((ILBRK.GT.0).AND.
     :     ((ILBRK.LE.1).OR.(IRBRK.LT.(ILBRK+2)))) THEN
         ERROR='Invalid use of parentheses'
         STATUS=DSA__OBJINV
      END IF
      IF (INBRK) THEN
         STATUS=DSA__OBJINV
         ERROR='Unbalanced parentheses'
      END IF
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     Now we attempt to break up the object name into its component bits
C     and put it together into the actual filename.
C
      IF (STATUS.EQ.0) THEN
         FULL_NAME=OBJECT_NAME
         EXT=' '
         STRUCT_NAME=' '
C
C        The structure name is anything following the first dot after
C        the last slash.
C
         IF (IDOT.GT.ISLASH) THEN
            STRUCT_NAME=OBJECT_NAME(IDOT:)
            FULL_NAME(IDOT:)=' '
         END IF
C
C        Anything within parentheses should be an explicit extension.
C
         IF (ILBRK.GT.0) THEN
            EXT=OBJECT_NAME(ILBRK+1:IRBRK-1)
            IF (EXT(1:1).NE.'.') THEN
               ERROR='"'//OBJECT_NAME(ILBRK+1:IRBRK-1)//
     :                      'is not a valid extension - needs a "."'
               STATUS=DSA__OBJINV
               GO TO 500    ! Error exit
            END IF
            FULL_NAME=FULL_NAME(:ILBRK-1)//FULL_NAME(IRBRK+1:)
         END IF
      END IF
C
C     Now that we've split up all the bits of the filename, may or may not
C     have an absolute file name - ie one that starts with '/'. If we don't
C     we will need to prepend the current default directory to the filename.
C
      IF (FULL_NAME(1:1).NE.'/') THEN
         CALL GEN_GETCWD(TRAN_NAME,STATUS)
         IF (STATUS.EQ.0) THEN
            FULL_NAME=TRAN_NAME(:ICH_LEN(TRAN_NAME))//'/'//FULL_NAME
         END IF
         STATUS=0
      END IF
C
C     That should leave us with the full expanded filename in FULL_NAME,
C     the structure name in STRUCT_NAME, and any explicit extension in EXT.
C     There is always the possibility that somewhere in all this there
C     are constructs such as 'dir/subdir/../file' which need to be
C     converted to 'dir/file'. We deal with that possibility here.
C
      IDOTS=INDEX(FULL_NAME,'../')
      DO WHILE (IDOTS.GT.0)
         ISLASH=0
         PSLASH=0
         DO I=1,IDOTS
            IF (FULL_NAME(I:I).EQ.'/') THEN
               PSLASH=ISLASH
               ISLASH=I
            END IF
         END DO
         IF (PSLASH.GT.0) FULL_NAME(PSLASH:)=FULL_NAME(IDOTS+2:)
         IDOTS=INDEX(FULL_NAME,'../')
      END DO
C
C     If the apparent structure name starts with a structure name that is
C     an allowed file extension, then the probability is that this is
C     intended to be an extension and we treat it as such. Moreover, if
C     the apparent extension includes a ',' followed by a number, then
C     we are almost certainly seeing a file name that includes an explicit
C     version number. We trap both these possibilities.
C
      FLEN=ICH_LEN(FULL_NAME)
      ISLEN=ICH_LEN(STRUCT_NAME)
      IEXEND=0
      IF ((ISLEN.GT.0).AND.(EXT.EQ.' ')) THEN
         ICOMMA=INDEX(STRUCT_NAME,',')
         IF (ICOMMA.GT.0) THEN
            IEXEND=ISLEN
            DO I=ICOMMA+1,ISLEN
               CHR=STRUCT_NAME(I:I)
               IF ((CHR.LT.'0').OR.(CHR.GT.'9')) THEN
                  IEXEND=I-1
                  GO TO 360  ! Break I loop
               END IF
            END DO
  360       CONTINUE
         ELSE
            DO IEXT=1,NEXTS
               LENEXT=ICH_LEN(DEFEXTS(IEXT))
               IF (STRUCT_NAME(:LENEXT).EQ.DEFEXTS(IEXT)) THEN
                  IF ((STRUCT_NAME(LENEXT+1:LENEXT+1).EQ.'.').OR.
     :                (STRUCT_NAME(LENEXT+1:LENEXT+1).EQ.' ')) THEN
                     IEXEND=LENEXT
                     GO TO 380         ! Break IEXT loop
                  END IF
               END IF
            END DO
  380       CONTINUE
         END IF
         IF (IEXEND.GT.0) THEN
            EXPEXT=.TRUE.
            FULL_NAME=FULL_NAME(:FLEN)//STRUCT_NAME(:IEXEND)
            FLEN=FLEN+IEXEND
            STRUCT_NAME=STRUCT_NAME(IEXEND+1:)
         END IF
      END IF
C
C     Now, if EXT contained an explicit file extension, we append that
C     to the file name to get the final version of the file name as
C     implied by OBJECT.
C
      IF (EXT.NE.' ') THEN
         EXPEXT=.TRUE.
         FULL_NAME=FULL_NAME(:FLEN)//EXT
         FLEN=FLEN+ICH_LEN(EXT)
      END IF
C
C     Having finished manipulating the structure name, we finally
C     return it in STRUCT and set it into upper case. (Note that we've
C     been using STRUCT_NAME rather than STRUCT to keep closer to the
C     letter of the Fortran 77 rule about manipulating passed-length
C     character strings.)
C
      STRUCT=STRUCT_NAME
      INVOKE=ICH_FOLD(STRUCT)
C
C     The rest would be easy, were it not for the possibility of
C     version names and the possibility of a second default file
C     extension and the chance that a file spec may be ambiguous
C     if files of the same name but both possible extensions exist.
C
      ALTFILE=' '
      IF (NEW) THEN
C
C        If the file is to be new, then there's little problem. If no
C        explicit extension was provided, we use the first default
C        extension. We then call VERSNAME to get the next version of
C        that file (if version numbers are supported).
C
         IF (.NOT.EXPEXT) THEN
            FULL_NAME(FLEN+1:)=DEFEXTS(1)
         END IF
         CALL DTA_VERSNAME(FULL_NAME,'NEW',FILENAME)
      ELSE
C
C        For an old file, it's trickier. If an explicit extension was
C        specified, then we use that and all we have to do is call
C        VERSNAME in case the latest version number has to be appended.
C
         IF (EXPEXT) THEN
            CALL DTA_VERSNAME(FULL_NAME,'OLD',FILENAME)
         ELSE
C
C           It's only this case, where an old file is wanted, but no
C           explicit extension has been specified, that can be tricky.
C           Even it is easy if only default extension is being used.
C           In that case, we use the first default extension, append the
C           version number and that should be that.
C
            FULL_NAME(FLEN+1:)=DEFEXTS(1)
            CALL DTA_VERSNAME(FULL_NAME,'OLD',FILE1)
            FILENAME=FILE1
            IF (NEXTS.GT.1) THEN
C
C              However, if two possible extensions are being used, we
C              need to do the same for the second possible extension and
C              then need to see if either (or both) of the two possible
C              files actually exist. If both do, we return one in FILENAME
C              and the other in ALTFILE. If only the second does, we use
C              that.
C
               FULL_NAME(FLEN+1:)=DEFEXTS(2)
               CALL DTA_VERSNAME(FULL_NAME,'OLD',FILE2)
               INQUIRE(FILE=FILE1(:ICH_LEN(FILE1)),EXIST=EXIST1,
     :                                                 IOSTAT=ISTAT)
               IF (ISTAT.NE.0) EXIST1=.FALSE.
               INQUIRE(FILE=FILE2(:ICH_LEN(FILE2)),EXIST=EXIST2,
     :                                                 IOSTAT=ISTAT)
               IF (ISTAT.NE.0) EXIST2=.FALSE.
               IF ((EXIST1.AND.EXIST2).AND.(FILE1.NE.FILE2)) THEN
                  FILENAME=FILE1
                  ALTFILE=FILE2
               ELSE IF (EXIST2.AND.(.NOT.EXIST1)) THEN
                  FILENAME=FILE2
               END IF
            END IF
         END IF
      END IF
C
C     And I think that's it!  All we do on the way out is close down
C     the EMS environment we set up at the start. If there was an error,
C     use ALTFILE to return the generated error message.
C
  500 CONTINUE
      IF (STATUS.NE.0) ALTFILE=ERROR
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
C
      END
