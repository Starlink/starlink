#if HAVE_CONFIG_H
#  include "config.h"
#endif
C+
C                    D S A Z _ T F O P E N
C
C  Routine name:
C     DSAZ_TFOPEN
C
C  Function:
C     Performs the system dependent part of opening a text file.
C
C  Description:
C     This routine will try to open either a new or existing text file.
C     It is passed a 'directory specification', which is a way of
C     specifying symbolically the name of a directory that the
C     file may be found in. This is intended to be something like
C     'FIGARO_PROG_L' and on a VAX will be a logical name, under UNIX
C     an environment variable, etc. This can be blank. The special case
C     where this is the single character '$' is taken as a reference to
C     the directory from which the current program is being run. Systems
C     where this concept cannot be implemented will simply not find
C     an existing file in this 'execute' directory and will not be able to
C     create a new file in it. The routine is also passed a file name,
C     which may be a completely specified file name, complete with
C     directory specification, file extension and version number (where
C     supported), or can be just the bare filename. It is also passed a
C     default file extension for use if this is not provided in the filename.
C     (If the filename appears to contain an extension, ie a dot within the
C     body of the filename, then the default extension will be ignored,
C     so a filename 'FILE.EXT' with a default extension of '.EXT' will
C     not be taken as the file 'FILE.EXT.EXT', even on a system where
C     this would be a valid name.) The default extension should begin
C     with a dot character, if specified. If it is blank, no extension
C     is assumed by this program. This routine respects the case of all these
C     strings and does not attempt to change them; whether the underlying
C     operating system respects filename case is not a question addressed
C     by this routine.  A file will be opened in the most suitable way for
C     a text file. The file can be specified as 'new' or 'not new' - ie
C     existing; a new file will always be opened for writing. An existing
C     file can be opened for writing or not; a file opened for reading will
C     be opened in such a way that other programs can share access to it.
C     Opening a 'new' file when a file of the same name exists will either
C     create a new version of the file or will replace the existing file -
C     this depends on the operating system. Note that the symbolic directory
C     is always used if it is non-blank; usually this program will be called
C     several times as a file is searched for, with various symbols for the
C     directory, and in one of these cases the symbolic directory will
C     normally be blank to allow the case where the filename itself
C     contains the directory specification to succeed.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSAZ_TFOPEN (LU,SYMDIR,FILENAME,DEFEXT,NEW,
C                                          WRITE,EXIST,ERROR,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LU       (Integer) The number of the Fortran logical
C                  unit to be used to access the file.
C     (>) SYMDIR   (character*(*)) The symbolic name for the directory.
C     (>) FILENAME (Character*(*)) The name of the file to be opened,
C                  either bare or with an explicit directory specification
C                  and or extension.
C     (>) DEFEXT   (Character*(*)) A string specifying the default
C                  extension for the file, including the leading dot.
C     (>) NEW      (Logical) True if a new file is to be created.
C     (>) WRITE    (Logical) True if an existing file is to be opened
C                  for writing.
C     (<) EXIST    (Logical) Independent of whether or not the file
C                  was opened, indicates if it existed or not.
C     (<) ERROR    (Character*(*)) A single string describing any error.
C     (!) STATUS   (Integer) Status value.  If this is passed as non
C                  zero, this routine returns immediately. If the file
C                  cannot be opened, this will be set to some non-zero
C                  code.
C
C  External variables used: None.
C
C  Prior requirements: None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  System:
C     This is the alpha version of this routine.
C
C  External subroutines / functions used:
C     EMS_ANNUL, EMS_BEGIN, EMS_END, ICH_LEN, PSX_GETENV
C
C  Subroutine / function details:
C     EMS_ANNUL    Clear any EMS error messages
C     EMS_BEGIN    Start a new EMS reporting environment
C     EMS_END      End the current EMS reporting environment
C     ICH_LEN      Position of last non-blank char in string
C     PSX_GETENV   Translates an environment variable
C
C  History:
C     24th Aug 1992  Original version.  KS / AAO.
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     19th Oct 1992  HME / UoE, Starlink.  Create different files for
C                    mips and sun4. Avoids preprocessor.
C      8th Apr 1993  HME / UoE, Starlink.  This IS the mips version,
C                    using READONLY. It was used until now on sun4
C                    instead of mips.
C                    Avoid INQUIRE.
C     12th Apr 1993  HME / UoE, Starlink.  Alpha version; use INQUIRE
C      1st JUl 2004  AA / Exeter, Starlink. Used as template to create
C                    FPP version of the file, will now be preprocessed
C                    and built by the autoconf system. The MIPS and
C                    VMS versions (which don't support INQUIRE) are no
C                    longer supported.
C+
      SUBROUTINE DSAZ_TFOPEN (LU,SYMDIR,FILENAME,DEFEXT,NEW,
     :                                       WRITE,EXIST,ERROR,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL NEW, WRITE, EXIST
      INTEGER LU, STATUS
      CHARACTER*(*) SYMDIR, FILENAME, DEFEXT, ERROR
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER   DLEN                 ! Number of characters in DEFEXT
      INTEGER   EMSTAT               ! Status used for EMS routine calls
      LOGICAL   EXT_GIVEN            ! True if extension specified in FILENAME
      INTEGER   FLEN                 ! Number of characters in FILENAME
      CHARACTER FULL_NAME*256        ! Name of file to be opened
      INTEGER   I                    ! Loop index through FILENAME chars
      INTEGER   ISTAT                ! Status of inquire operation
      INTEGER   PSX_STATUS           ! Status used for PSX routine calls
      INTEGER   SLEN                 ! Number of characters in SYMDIR
      CHARACTER TRAN_NAME*256        ! Translated version of SYMDIR
      INTEGER   TLEN                 ! Number of characters in TRAN_NAME
C
      IF (STATUS.NE.0) RETURN
C
C     Set new EMS reporting environment (since PSX routines report
C     errors using EMS and we want them kept quiet).
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Under UNIX we don't handle the special case where '$' is used for
C     the symbolic directory.
C
      IF (SYMDIR.EQ.'$') THEN
         ERROR='"Execution directory" not available under UNIX'
         STATUS=1
         EXIST=.FALSE.
         GO TO 500        ! Error exit
      END IF
C
C     Generate the filename to use.  This involves looking at FILENAME and
C     pre-pending the translated symbolic directory name (allowing for
C     a '/' if this seems necessary). If FILENAME seems not to contain an
C     extension already, we append the specified default.
C
      FLEN=ICH_LEN(FILENAME)
C
C     Start by looking back through FILENAME to see if there is a '.'
C     in the last part of the name - up to the last '/'.
C
      EXT_GIVEN=.FALSE.
      DO I=FLEN,1,-1
         IF (FILENAME(I:I).EQ.'/') GO TO 320    ! Break loop on '/'
         IF (FILENAME(I:I).EQ.'.') THEN
            EXT_GIVEN=.TRUE.
            GO TO 320           ! Break loop on '.'
         END IF
      END DO
  320 CONTINUE
C
C     Now, if the symbolic directory name is not blank, treat it as an
C     environment variable and translate it. If it doesn't translate
C     treat this as an error. Pre-pend the result to FILENAME, inserting
C     a '/' if the translation doesn't end with this.
C
      FULL_NAME=FILENAME
      SLEN=ICH_LEN(SYMDIR)
      IF (SLEN.GT.0) THEN
         PSX_STATUS=0
         CALL PSX_GETENV (SYMDIR(:SLEN),TRAN_NAME,PSX_STATUS)
         IF (PSX_STATUS.NE.0) THEN
            STATUS=1
            EXIST=.FALSE.
            ERROR='Cannot translate "'//SYMDIR(:SLEN)//'"'
            GO TO 500         ! Error exit
         END IF
         TLEN=ICH_LEN(TRAN_NAME)
         IF (TLEN.GT.0) THEN
            IF (TRAN_NAME(TLEN:TLEN).EQ.'/') THEN
               FULL_NAME=TRAN_NAME(:TLEN)//FILENAME
               FLEN=FLEN+TLEN
            ELSE
               FULL_NAME=TRAN_NAME(:TLEN)//'/'//FILENAME
               FLEN=FLEN+TLEN+1
            END IF
         END IF
      END IF
C
C     Finally, append the extension, if one was given in DEFEXT and
C     no extension seemed to be specified in FILENAME.
C
      DLEN=ICH_LEN(DEFEXT)
      IF ((DLEN.GT.0).AND.(.NOT.EXT_GIVEN)) THEN
         FULL_NAME(FLEN+1:)=DEFEXT
         FLEN=FLEN+DLEN
      END IF
C
C     First, see if the file exists - we need to know this to report
C     using EXIST, even if a new version is to be created.
C
      INQUIRE (FILE=FULL_NAME(:FLEN),EXIST=EXIST,IOSTAT=ISTAT)

      IF (ISTAT.NE.0) EXIST=.FALSE.
C
      IF (NEW) THEN
C
C        If the file is to be created, see if we can create it.
C        Under UNIX, an open using 'NEW' will fail if a file of the
C        same name already exists.
C
         IF (EXIST) THEN
            OPEN (UNIT=LU,FILE=FULL_NAME(:FLEN),STATUS='OLD',
     :                                              IOSTAT=STATUS)
         ELSE
            OPEN (UNIT=LU,FILE=FULL_NAME(:FLEN),STATUS='NEW',
     :                                              IOSTAT=STATUS)
         END IF
         IF (STATUS.NE.0) THEN
            CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
         END IF
      ELSE
C
C        The file should already exist. See if it did - this assumes that
C        this routine will be called more frequently for a file that doesn't
C        exist than for one that does, and that the INQUIRE is faster than
C        a tentative OPEN. (Otherwise, we'd only do the INQUIRE for the
C        'new' case and just use an OPEN here.) If it does exist, try to
C        open it.
C
         IF (EXIST) THEN
            IF (WRITE) THEN
               OPEN (UNIT=LU,FILE=FULL_NAME(:FLEN),STATUS='OLD',
     :                                               IOSTAT=STATUS)
            ELSE
               OPEN (UNIT=LU,FILE=FULL_NAME(:FLEN),STATUS='OLD',
#if HAVE_FC_OPEN_ACTIONREAD
     :               ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :                                      READONLY,
#endif
     :              IOSTAT=STATUS)
            END IF
            IF (STATUS.NE.0) THEN
               CALL GEN_FORTERR(STATUS,.FALSE.,ERROR)
            END IF
         ELSE
            STATUS=1
            ERROR='File does not exist'
         END IF
      END IF
C
  500 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
C
      END
