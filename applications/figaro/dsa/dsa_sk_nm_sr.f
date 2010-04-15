C+
C           D S A _ S E E K _ N A M E D _ S T R U C T U R E
C
C  Routine name:
C     DSA_SEEK_NAMED_STRUCTURE
C
C  Function:
C     Looks to see if a structure with a given name already exists.
C
C  Description:
C     This routine can be used to see if a structure already exists.
C     It should be passed the name explicitly, in the form that would
C     be passed to, say, DSA_NAMED_INPUT.  In fact, it's function can
C     be described as to determine whether or not DSA_NAMED_INPUT would
C     be able to open the specified structure.  It is intended for cases
C     where one needs to be able to determine whether DSA_INPUT or
C     DSA_OUTPUT should be used to open a structure.  A message is output
C     and bad status returned if the structure name cannot be parsed
C     properly, but the non-existence of a validly named structure is
C     not regarded as an error.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SEEK_NAMED_STRUCTURE (STRUCTURE,EXIST,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) STRUCTURE       (Fixed string,descr) The structure name.  This
C                         can be just a file name, or it may have a sub-
C                         structure name included as well, as in
C                         DISK$DATA:[DIR]FILENAME.STRUCT.
C     (<) EXIST           (Logical, ref) Returned true if the file
C                         exists and if any specified structure exists
C                         within the file.
C     (!) STATUS          (Integer, ref) Status code.  If bad status is
C                         passed to this routine, it returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DTA_ASFNAM, DTA_FCLOSE, DTA_TYVAR, DSA_FNAME, GEN_EXIST.
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DTA_ASFNAM     Opens a data structure file.
C     DTA_TYVAR      Get type of a data object (used to check existence)
C     DTA_FCLOSE     Close a data structure file.
C     DSA_FNAME      Parse a structure name into file and structure.
C     GEN_EXIST      See if a named file exists.
C
C  History:
C     25th Jan 1989.   Original version.  KS / AAO.
C     15th Feb 1991.   Call to DSA_FNAME now has extension parameter. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SEEK_NAMED_STRUCTURE (STRUCTURE,EXIST,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      LOGICAL EXIST
      CHARACTER*(*) STRUCTURE
C
C     Functions used
C
      LOGICAL GEN_EXIST
C
C     Local variables
C
      INTEGER   DTA_STATUS          ! Status returned by DTA routine
      CHARACTER FILENAME*128        ! Name of file
      INTEGER   IGNORE              ! Status - ignored
      CHARACTER STRUCT*64           ! Name of structure within file
      CHARACTER TYPE*16             ! Type of object - ignored
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Parse the structure name into file and structure components.
C
      CALL DSA_FNAME (STRUCTURE,.FALSE.,' ',FILENAME,STRUCT,STATUS)
      IF (STATUS.EQ.0) THEN
C
C        See if the file exists.  If no structure has been specified,
C        that's all we need to knwo.  If there was a structure then we
C        have to open the file to see if the structure does exist,
C        which is rather a shame, but that's the best we can do.
C
         IF (GEN_EXIST(FILENAME)) THEN
            IF (STRUCT.EQ.' ') THEN
               EXIST=.TRUE.
            ELSE
               CALL DTA_ASFNAM('DSA_TEMP',FILENAME,'OLD',0,' ',
     :                                                      DTA_STATUS)
               IF (DTA_STATUS.EQ.0) THEN
                  CALL DTA_TYVAR('DSA_TEMP'//STRUCT,TYPE,DTA_STATUS)
                  CALL DTA_FCLOSE('DSA_TEMP',IGNORE)
               END IF
               EXIST=DTA_STATUS.EQ.0
            END IF
         ELSE
            EXIST=.FALSE.
         END IF
      END IF
C
      END
