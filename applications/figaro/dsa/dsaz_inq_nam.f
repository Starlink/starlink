C+
C                D S A Z _ I N Q _ N A M E
C
C  Routine name:
C     DSAZ_INQ_NAME
C
C  Function:
C     Gets the full path name of a specified file.
C
C  Description:
C     This routine returns the full name of a file. The intention is that
C     this full name can be used either to produce a message to a user
C     showing exactly which file is being used, or can be used to provide
C     an unambiguous way to identify the file - a call to this routine,
C     no matter how the file has been specified, will always return the
C     same name string for the same file, and this can be used to see,
C     for example, if a specified file is already known to the program,
C     even though its name may have been given in some different form
C     when it was previously accessed.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSAZ_INQ_NAME (FILENAME,LU,FULLNAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) FILENAME (Character*(*)) The name of the file in question.
C                  This should be such that it could be specified in a
C                  Fortran OPEN statement - ie anything that will work
C                  for an OPEN statement will work here.  Can be blank,
C                  in which case the file is assumed to be already open
C                  on the logical unit given by LU.
C     (>) LU       (Integer) A Fortran logical unit number on which
C                  the file is already open.  Only used if FILENAME is
C                  blank.
C     (<) FULLNAME (Character*(*)) The full name of the file in question.
C     (!) STATUS   (Integer) Status value.  If this is passed as non
C                  zero, this routine returns immediately. If an error
C                  occurs this will be set to some non-zero code, the
C                  value of which will be system-specific.
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
C     This is the UNIX version of this routine.
C
C  External subroutines / functions used:
C     EMS_ANNUL, EMS_BEGIN, EMS_END, ICH_LEN, GEN_GETCWD
C
C  Subroutine / function details:
C     EMS_ANNUL    Clear any EMS error messages
C     EMS_BEGIN    Start a new EMS reporting environment
C     EMS_END      End the current EMS reporting environment
C     ICH_LEN      Position of last non-blank char in string
C     GEN_GETCWD   Gets absolute pathname of current working directory
C
C  History:
C     26th Aug 1992  Original version.  KS / AAO.
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     28th Jul 1993  HME / UoE, Starlink.  Changed PSX_GETCWD to GEN_*.
C+
      SUBROUTINE DSAZ_INQ_NAME (FILENAME,LU,FULLNAME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER LU, STATUS
      CHARACTER*(*) FILENAME, FULLNAME
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     Local variables
C
      CHARACTER CWD*256             ! Current directory
      INTEGER   EMSTAT              ! Status passed to EMS routines
      INTEGER   FLEN                ! Number of characters in FILENAME
      INTEGER   I                   ! Loop index through file name
      INTEGER   IDOT                ! Position of any '../' sequence
      INTEGER   ISLASH              ! Position of penultimate '/' so far
      INTEGER   LSLASH              ! Position of last '/' so far
      CHARACTER TEMP_NAME*256       ! Work string - keeps to F77 standard
C
      IF (STATUS.NE.0) RETURN
C
C     Under UNIX, a simple Fortran INQUIRE statement will give the file
C     name, but some implementations will return an absolute pathname,
C     others a relative pathname. We can distinguish the two just by
C     looking at the first character.
C
      FLEN=ICH_LEN(FILENAME)
      IF (FLEN.EQ.0) THEN
         INQUIRE (UNIT=LU,NAME=TEMP_NAME,IOSTAT=STATUS)
      ELSE
         INQUIRE (FILE=FILENAME,NAME=TEMP_NAME,IOSTAT=STATUS)
      END IF
C
      IF (STATUS.NE.0) GO TO 500      ! Error exit
C
      IF (TEMP_NAME(1:1).NE.'/') THEN
C
C        This implementation has given us a relative pathname. We need
C        to get the full pathname by combining this with the current
C        working directory name. We get this through a call to GEN_GETCWD.
C        Note that since PSX routines use EMS to log errors we need to
C        control that. (GEN_GETCWD is inteded to become a PSX routine.)
C
         EMSTAT=0
         CALL EMS_BEGIN(EMSTAT)
         CALL GEN_GETCWD(CWD,STATUS)
         EMSTAT=0
         CALL EMS_ANNUL(EMSTAT)
         CALL EMS_END(EMSTAT)
C
C        Now we combine the two parts of the name to get the full filename.
C
         TEMP_NAME=CWD(:ICH_LEN(CWD))//'/'//TEMP_NAME
      END IF
C
C     Unfortunately, it seems that in some implementations the INQUIRE
C     statement will return a string that is just the same as the string
C     used to open the file (the SUN does this). In this cases, you can
C     get filenames as perverse as '../other_dir/../this_dir/file.ext'.
C     If we have a resulting string that contains '..' characters in it,
C     we need to remove each instance, together with the preceding
C     directory name. We start again each time through, just in case of
C     constructs such as '/home/aaossg/ks/dsa/../../user/dir/file.ext'
C     and the like.
C
      IDOT=INDEX(TEMP_NAME,'../')
      DO WHILE (IDOT.GT.0)
         ISLASH=0
         LSLASH=0
         DO I=1,IDOT
            IF (TEMP_NAME(I:I).EQ.'/') THEN
               ISLASH=LSLASH
               LSLASH=I
            END IF
         END DO
         IF (ISLASH.GT.0) TEMP_NAME(ISLASH:)=TEMP_NAME(IDOT+2:)
         IDOT=INDEX(TEMP_NAME,'../')
      END DO
C
  500 CONTINUE
      FULLNAME=TEMP_NAME
C
      END
