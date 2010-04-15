C+
C                    D S A _ O P E N _ T E X T _ F I L E
C
C  Routine name:
C     DSA_OPEN_TEXT_FILE
C
C  Function:
C     Opens a named text file, either new or existing.
C
C  Description:
C     This routine will open either a new or existing text file.
C     It returns both the full name of the opened file and a Fortran
C     logical unit number that can be used to access it.  If the
C     file is new, or if the name includes a directory specification
C     or a logical name then the file name supplied will be used unchanged.
C     If the file is supposed to exist, and no directory name is specified,
C     then it will be searched for in the standard Figaro sequence, that
C     is, 1) default directory, 2) FIGARO_PROG_U, 3) the .EXE directory
C     (that is, the directory from which the current .EXE image comes,
C     if the system supports this concept) 4) FIGARO_PROG_L, 5)
C     FIGARO_PROG_N, 6) FIGARO_PROG_S.  Read-only files are opened shared.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_OPEN_TEXT_FILE (FILE,DEFAULT,EXIST,WRITE,LU,NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) FILE     (Character,descr) The name of the file to be opened.
C     (>) DEFAULT  (Character,descr) A string specifying the default
C                  name for the file.  This is to be used for supplying
C                  a default extension, and should be just that default
C                  extension, including the leading dot.
C     (>) EXIST    (Character,descr) A string specifying whether the
C                  file is supposed to exist or not.  This string should
C                  be one of 'NEW', 'OLD', or 'UNKNOWN' -  the case is
C                  not significant.
C     (>) WRITE    (Logical) True if the file is to be written to.
C     (<) LU       (Integer,ref) The number of a reserved Fortran logical
C                  unit that can be used to access the file.
C     (<) NAME     (Character,descr) The full name of the opened file.
C     (!) STATUS   (Integer,ref) Status value.  If this is passed as non
C                  zero, this routine returns immediately.
C
C  External variables used:
C     Common variables used only by the DSA_ package.
C
C  External subroutines / functions used:
C     GEN_FORTERR, ICH_LEN, ICH_FOLD, DSA_WRUSER, DSA_GET_LU,
C     DSA_FREE_LU, DSAZ_TFOPEN, DSAZ_INQ_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C  Note:
C     The original VAX version of this routine allowed DEFAULT to specify
C     a general default file specification. For the portable version of
C     DSA this has been restricted and only a default extension is supported.
C     (In practice, the VAX implementation will still work as it used to,
C     but using more than an extension is not supported on all platforms.)
C-
C  Common variable details:
C     (<) FOR_CODE     (Integer) Last Fortran error code.
C
C  Subroutine / function details:
C     GEN_FORTERR   Gets error text for a Fortran I/O status code
C     ICH_LEN       Position of last non-blank char in string
C     ICH_FOLD      COnvert a string to upper case.
C     DSA_WRUSER    Write message string to user.
C     DSA_WRFLUSH   Flush messages to user.
C     DSAZ_TFOPEN   Open a file (handles system dependent code).
C     DSAZ_INQ_NAME Get full path name of an opened file.
C     DSA_GET_LU    Reserve an unused logical unit number
C     DSA_FREE_LU   Free a previously reserved logical unit number
C
C  History:
C     3rd March 1988  Original version.  KS / AAO.
C     7th Sept  1988  Search path modified to include .EXE and the
C                     national directory.  Also now uses GEN_EXIST.
C                     KS/AAO.
C     15th Sept 1988  DEFAULT added to call to GEN_EXIST.  KS/AAO.
C     21st Aug  1992  Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     26th Aug  1992  Recoded to remove non-portable code to the various
C                     DSAZ_ routines. KS/AAO.
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_OPEN_TEXT_FILE (FILE,DEFAULT,EXIST,WRITE,LU,
     :                                                  NAME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL WRITE
      INTEGER LU, STATUS
      CHARACTER*(*) FILE, DEFAULT, EXIST, NAME
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     Local variables
C
      LOGICAL   DID_EXIST        ! True if the file did exist
      CHARACTER ERROR*64         ! Error text
      CHARACTER EXSTR*8          ! Upper case version of EXIST
      LOGICAL   GOTLU            ! Flags that a logical unit was reserved
      INTEGER   I                ! Loop index
      INTEGER   IDIR             ! Index of directory file was found in
      INTEGER   IGNORE           ! Dummy status value - ignored
      INTEGER   INVOKE           ! Dummy function value
      LOGICAL   MAY_EXIST        ! Indicates 'OLD' or 'UNKNOWN' specified
      LOGICAL   OPENED           ! True once file opened
C
C     DSA_ system common data
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Symbolic names of directories to search. ('$' is used by DSAZ_TFOPEN
C     to indicate the .EXE directory), and the blank string will have the
C     effect of referring to the default directory.
C
      CHARACTER PREFIX(6)*13
      DATA PREFIX/'             ','FIGARO_PROG_U','$            ',
     :            'FIGARO_PROG_L','FIGARO_PROG_N','FIGARO_PROG_S'/
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Initial values
C
      GOTLU = .FALSE.
      OPENED = .FALSE.
C
C     Get a logical unit number to use
C
      CALL DSA_GET_LU (LU,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
      GOTLU = .TRUE.
C
C     Check the value passed for EXIST
C
      EXSTR = EXIST
      INVOKE = ICH_FOLD (EXSTR)
      IF ((EXSTR.EQ.'OLD').OR.(EXSTR.EQ.'UNKNOWN')) THEN
         MAY_EXIST = .TRUE.
      ELSE IF (EXSTR.EQ.'NEW') THEN
         MAY_EXIST = .FALSE.
      ELSE
         CALL DSA_WRUSER ('Cannot open the file "')
         CALL DSA_WRUSER (FILE(:ICH_LEN(FILE)))
         IF (DEFAULT.NE.' ') THEN
            CALL DSA_WRUSER ('", default "')
            CALL DSA_WRUSER (DEFAULT(:ICH_LEN(DEFAULT)))
         END IF
         CALL DSA_WRUSER ('", since "')
         CALL DSA_WRUSER (EXSTR(:ICH_LEN(EXSTR)))
         CALL DSA_WRUSER ('" does not specify its existence properly. ')
         CALL DSA_WRUSER ('Programming error.')
         CALL DSA_WRFLUSH
         STATUS = DSA__INVEXS
         GO TO 500              ! Error exit
      END IF
C
      IF (MAY_EXIST) THEN
C
C        If the file may exist, we have to look for it.  We try the
C        various directories one by one.  If we actually fail to
C        open an existing file, we exit the loop.
C
         DO I = 1,6
            STATUS=0
            CALL DSAZ_TFOPEN (LU,PREFIX(I),FILE,DEFAULT,.FALSE.,WRITE,
     :                                         DID_EXIST,ERROR,STATUS)
            IF (STATUS.EQ.0.OR.DID_EXIST) THEN
               IDIR=I
               GO TO 320    ! Break out of I loop
            END IF
         END DO
  320    CONTINUE
C
         IF (STATUS.EQ.0) THEN
            OPENED=.TRUE.
         ELSE
C
            IF (DID_EXIST) THEN
C
C              If the file did exist and we didn't open it - a protection
C              problem, probably - then we treat that as an error, even if
C              we are allowed to create a new file.
C
               FOR_CODE = STATUS
               STATUS = DSA__FORERR
               CALL DSA_WRUSER ('The file "')
               CALL DSA_WRUSER (FILE(:ICH_LEN(FILE)))
               IF (DEFAULT.NE.' ') THEN
                  CALL DSA_WRUSER ('", default "')
                  CALL DSA_WRUSER (DEFAULT(:ICH_LEN(DEFAULT)))
               END IF
               CALL DSA_WRUSER ('" ')
               IF (IDIR.EQ.1) THEN
                  CALL DSA_WRUSER('in the default directory')
               ELSE IF (PREFIX(IDIR).EQ.'$') THEN
                  CALL DSA_WRUSER('in the "execution" directory')
               ELSE
                  CALL DSA_WRUSER('in the directory "'//PREFIX(IDIR)
     :                                                        //'"')
               END IF
               CALL DSA_WRUSER(' could not be opened')
               IF (WRITE) CALL DSA_WRUSER(' for writing')
               CALL DSA_WRUSER('. ')
               CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRFLUSH
               GO TO 500     ! Error exit
C
            ELSE IF (EXSTR.EQ.'OLD') THEN
C
C              Here, it didn't exist, but it was supposed to, so
C              that's obviously an error.
C
               FOR_CODE = STATUS
               STATUS = DSA__FORERR
               CALL DSA_WRUSER ('Failed to find the file "')
               CALL DSA_WRUSER (FILE(:ICH_LEN(FILE)))
               IF (DEFAULT.NE.' ') THEN
                  CALL DSA_WRUSER ('", default "')
                  CALL DSA_WRUSER (DEFAULT(:ICH_LEN(DEFAULT)))
               END IF
               CALL DSA_WRUSER ('" in any of the Figaro directories, ')
               CALL DSA_WRUSER ('or in the default directory.')
               CALL DSA_WRFLUSH
               GO TO 500     ! Error exit
            END IF
         END IF
      END IF
C
C     If we have got here, then there has been no error so far. Either
C     we have opened an existing file, or the file didn't exist and
C     DEFAULT was either 'NEW' or 'UNKNOWN' so we can try to create a
C     new one. A new file is always opened just by name as it stands.
C     Note that we don't even bother to test the value of WRITE - we
C     assume a new file is not to be opened readonly.
C
      IF (.NOT.OPENED) THEN
         STATUS=0
         CALL DSAZ_TFOPEN (LU,' ',FILE,DEFAULT,.TRUE.,.TRUE.,
     :                                       DID_EXIST,ERROR,STATUS)
         IF (STATUS.NE.0) THEN
            FOR_CODE = STATUS
            STATUS = DSA__FORERR
            CALL DSA_WRUSER ('Failed to create the file "')
            CALL DSA_WRUSER (FILE(:ICH_LEN(FILE)))
            IF (DEFAULT.NE.' ') THEN
               CALL DSA_WRUSER ('", default "')
               CALL DSA_WRUSER (DEFAULT(:ICH_LEN(DEFAULT)))
            END IF
            CALL DSA_WRUSER ('". ')
            CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER ('.')
            IF (MAY_EXIST) THEN
               CALL DSA_WRUSER(' (Having also failed to find an ')
               CALL DSA_WRUSER('existing file in the various Figaro'//
     :                                               ' directories.)')
            END IF
            CALL DSA_WRFLUSH
            GO TO 500     ! Error exit
         END IF
      END IF
C
C     If we got here, we have an open file.  Get it's full name.
C
      CALL DSAZ_INQ_NAME (' ',LU,NAME,IGNORE)
C
C     Exit.  If we got a logical unit number but failed to open the
C     file, release it.
C
  500 CONTINUE
      IF (GOTLU.AND.(STATUS.NE.0)) THEN
         IGNORE = 0
         CALL DSA_FREE_LU (LU,IGNORE)
      END IF
C
      END
