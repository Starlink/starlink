C+
      SUBROUTINE DSA_WRNAME (NAME)
C
C                            D S A _ W R N A M E
C
C  Routine name:
C     DSA_WRNAME
C
C  Function:
C     Outputs the full name of a structure, including its file name.
C
C  Description:
C     Given the DTA_ system name of a data object, which is a combination
C     of the internal top level name used by the system and a structured
C     name, this routine outpus the name in a way that replaces the internal
C     top level name (which the user will, in general, not recognise),
C     with a reference to the filename.   For example, if NAME is, say,
C     'IMAGE.Z.DATA', this routine will output '.Z.DATA in the file xxxx'.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_WRNAME (NAME)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME       (Fixed string,descr) The DTA_ system name of the
C                    object.  Must be in upper case.
C
C  External variables used:
C     Common variables used internally by the DSA_ routines
C
C  External subroutines / functions used:
C     ICH_LEN, DSA_WRUSER
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) MAX_FILES     (Integer parameter) Maximum number of file entries.
C     (>) FILE_NAMES    (String array) Full file specification for each file.
C     (>) FILE_TOP_NAMES (String array) Top-level name associated with
C                       each file.
C     (>) FILE_USED     (Logical array) Indicates file table slot in use.
C
C  Subroutine / function details:
C     ICH_LEN        Position of last non-blank char in string
C     DSA_WRUSER     Output message to user
C
C  History:
C     6th July 1987   Original version.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) NAME
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA_ system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   I                          ! Loop index through file slots
      INTEGER   IDOT                       ! Position of first '.' in NAME
      INTEGER   IFILE                      ! File slot number for name
      INTEGER   ITOP                       ! Length of top level part of NAME
      INTEGER   LENGTH                     ! Number of characters in NAME
C
C     Break the name just after the top level part.
C
      LENGTH=ICH_LEN(NAME)
      IDOT=INDEX(NAME,'.')
      IF (IDOT.EQ.0) THEN
         ITOP=LENGTH
      ELSE
         ITOP=IDOT-1
      END IF
C
C     Get the file name corresponding to that top-level name
C
      IFILE=0
      DO I=1,MAX_FILES
         IF (FILE_USED(I)) THEN
            IF (FILE_TOP_NAMES(I).EQ.NAME(:ITOP)) THEN
               IFILE=I
               GO TO 340        ! Break out of loop
            END IF
         END IF
      END DO
  340 CONTINUE
C
C     Now format the output string and output it.  If there isn't a file,
C     we can't do very much other than just output NAME as it stands.
C     If there is, we can go into more detail.
C
      IF (IFILE.EQ.0) THEN
         CALL DSA_WRUSER(NAME(:LENGTH))
      ELSE
         IF (IDOT.EQ.0) THEN
            CALL DSA_WRUSER('the main object ')
         ELSE
            CALL DSA_WRUSER(NAME(IDOT:LENGTH))
         END IF
         CALL DSA_WRUSER(' in the file ')
         CALL DSA_WRUSER(FILE_NAMES(IFILE)(:ICH_LEN(FILE_NAMES(IFILE))))
      END IF
C
      END
