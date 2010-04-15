C+
C           D S A _ S E E K _ S T R U C T U R E
C
C  Routine name:
C     DSA_SEEK_STRUCTURE
C
C  Function:
C     Looks to see if a structure specified by a parameter already exists.
C
C  Description:
C     This routine can be used to see if a structure already exists.
C     It should be passed the name of a parameter system parameter,
C     which should be a character or file parameter.  It uses the
C     parameter system to get the value of that parameter and checks to
C     see if that parameter specifies an existing structure - ie one
C     that coul dbe opened by, say, DSA_INPUT.  In fact, it's function
C     can be described as being to determine whether or not DSA_NAMED_INPUT
C     would be able to open the specified structure.  It is intended
C     for cases where one needs to be able to determine whether
C     DSA_INPUT or DSA_OUTPUT should be used to open a structure.  The
C     routine interacts with the parameter system to attempt to get a
C     parameter value that can at least be parsed properly, and then
C     returns that value as well as an indication as to whether it
C     already exists or not.  The usual way of using this routine is to
C     use it to get the parameter value and then to pass that to either
C     DSA_NAMED_INPUT or DSA_NAMED_OUTPUT, depending on whether or not
C     the structure exists.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SEEK_STRUCTURE (PARM_NAME,EXIST,STRUCTURE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) PARM_NAME       (Fixed string, descr) The parameter name.
C     (<) EXIST           (Logical, ref) Returned true if the file
C                         exists and if any specified structure exists
C                         within the file.
C     (<) STRUCTURE       (Fixed string,descr) The structure name.  This
C                         can be just a file name, or it may have a sub-
C                         structure name included as well, as in
C                         DISK$DATA:[DIR]FILENAME.STRUCT.
C     (!) STATUS          (Integer, ref) Status code.  If bad status is
C                         passed to this routine, it returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     ICH_LEN, DSA_SEEK_NAMED_STRUCTURE, PAR_ABORT, PAR_CNPAR
C     PAR_RDCHAR, PAR_SDCHAR
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN      Position of last non-blank char in string
C     PAR_ABORT    Indicates if abort flag has been set
C     PAR_CNPAR    Cancels the value of a parameter - forces reprompting
C     PAR_SDCHAR   Sets the default value of a parameter
C     PAR_RDCHAR   Get the value of a character parameter
C     DSA_SEEK_NAMED_STRUCTURE  Check on existence of a named structure
C
C  History:
C     25th Jan 1989.   Original version.  KS / AAO.
C     8th  Dec 1989.   Minor typo in comments fixed.  KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     25th Jul 1996    Catenation for Linux.  MJCL/Starlink, UCL.
C+
      SUBROUTINE DSA_SEEK_STRUCTURE (PARM_NAME,EXIST,STRUCTURE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      LOGICAL EXIST
      CHARACTER*(*) PARM_NAME, STRUCTURE
C
C     Functions used
C
      LOGICAL PAR_ABORT, PAR_BATCH
      INTEGER ICH_LEN
C
C     DSA system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      LOGICAL   BATCH                           ! True if in batch mode
      INTEGER   IGNORE                          ! Ignored status code
      LOGICAL   RETRY                           ! Controls retry loop
      CHARACTER STRING*80                       ! Local string storage
C
C     Check for bad passed status
C
      IF (STATUS.NE.0) RETURN
C
C     The following 'get parameter, see if exists, cancel and
C     retry on parse failure' loop continues until a satisfactory
C     parameter value is obtained.  In batch mode, we don't have that
C     option, of course.
C
      BATCH=PAR_BATCH()
      RETRY=.TRUE.
      DO WHILE (RETRY)
         STRING=PARM_NAME(:ICH_LEN(PARM_NAME))//'/NOCHECK'
         CALL PAR_RDCHAR (STRING,' ',STRUCTURE)
         IF (PAR_ABORT()) THEN
            STATUS=DSA__ABORT
            RETRY=.FALSE.
         ELSE
            CALL DSA_SEEK_NAMED_STRUCTURE (STRUCTURE,EXIST,STATUS)
            IF ((STATUS.NE.0).AND.(.NOT.BATCH)) THEN
               CALL PAR_CNPAR (PARM_NAME)
               CALL PAR_SDCHAR(PARM_NAME,' ',IGNORE)
               STATUS=0
            ELSE
               RETRY=.FALSE.
            END IF
         END IF
      END DO
C
      END
