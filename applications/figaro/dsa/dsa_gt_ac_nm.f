C+
C                   D S A _ G E T _ A C T U A L _ N A M E
C
C  Routine name:
C     DSA_GET_ACTUAL_NAME
C
C  Function:
C     Returns the actual structure name corresponding to a reference name.
C
C  Description:
C     The 'actual structure name' for a data structure is a full name
C     that completely defines the location of the structure, and is in
C     a form that could be used for input to, for example, DSA_NAMED_INPUT.
C     It contains the disk, directory, filename, extension, version number
C     and if a structure other than that at the top of the file is meant,
C     the structure information as well.  This routine returns the actual
C     structure name corresponding to a reference name that has already
C     been associated with a structure by one of the DSA_ file opening
C     routines.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE_NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME        (Fixed string,descr) The reference name in question.
C     (<) STRUCTURE_NAME  (Fixed string,descr) The corresponding actual
C                         structure name.
C     (!) STATUS          (Integer,ref) Status code.  If a bad status value
C                         is passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ routines.
C
C  External subroutines / functions used:
C     DSA_FIND_REF, DSA_WRUSER, ICH_FOLD, ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ routines.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) ACTUAL_NAMES  (String array) Actual names corresponding to ref names.
C
C  Subroutine / function details:
C     ICH_LEN       Position of last non-blank char in string.
C     ICH_FOLD      Convert string to upper case.
C     DSA_FIND_REF  Look up reference name in DSA_ common tables.
C
C  History:
C     16th June 1987.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE_NAME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME, STRUCTURE_NAME
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ system common variables
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   LENGTH                         ! Length of OBJ_NAME - ignored.
      INTEGER   LENREF                         ! Length of REF_NAME
      CHARACTER OBJ_NAME*8                     ! Object name - ignored.
      CHARACTER REF_NAME_UC*32                 ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                       ! Slot # in ref name tables
C
C     Return on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     We need to use an upper case version of REF_NAME
C
      REF_NAME_UC=REF_NAME
      LENREF=ICH_FOLD(REF_NAME_UC)
C
C     Look up the reference name in the tables
C
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      IF (STATUS.EQ.0) THEN
C
C        Return the actual name from the tables
C
         STRUCTURE_NAME=ACTUAL_NAMES(REF_SLOT)
         IF (ICH_LEN(ACTUAL_NAMES(REF_SLOT)).GT.LEN(STRUCTURE_NAME))
     :                                                           THEN
            CALL DSA_WRUSER('Significant characters are being missed ')
            CALL DSA_WRUSER(
     :         'from the end of the actual name corresponding to "')
            CALL DSA_WRUSER(REF_NAME_UC(:LENREF))
            CALL DSA_WRUSER('".  Programming error.')
            CALL DSA_WRFLUSH
            STATUS=DSA__STRTRN
         END IF
      END IF
C
      END
