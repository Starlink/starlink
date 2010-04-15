C+
C                       D S A _ F I N D _ R E F
C
C  Routine name:
C     DSA_FIND_REF
C
C  Function:
C     Looks up a reference name in the DSA_ system common tables
C
C  Description:
C     DSA_FIND_REF looks up a reference name in the common tables
C     maintained by the DSA_ system, and returns its slot number
C     and the DTA_ object name corresponding to the reference name.
C     This routine assumes that the reference name is in the tables,
C     and outputs an error message if this is not the case.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_FIND_REF (REF_NAME,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name.  This
C                        should be in upper case.
C     (<) REF_SLOT       (Integer,ref) The slot number for the name.
C     (<) OBJ_NAME       (Fixed string,descr) The DTA_ object name
C                        corresponding to REF_NAME.
C     (<) LENGTH         (Integer,ref) The number of non-blank characters
C                        in OBJ_NAME.
C     (!) STATUS         (Integer,ref) Status code.  If a bad status value
C                        is passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ package.
C
C  External subroutines / functions used:  DSA_WRUSER, ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C  Note:
C     This is an internal DSA_ system routine and should not be called
C     directly from outside the DSA_ system.
C-
C  Common variable details:
C     (>) MAX_REFS    (Integer parameter) Maximum number of reference names.
C     (>) OBJ_LEN     (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES   (String array) Name (as recognised by DTA_) of data
C                     object corresponding to reference name.
C     (>) REF_NAMES   (String array) Reference names in use.
C     (>) REF_USED    (Logical array) Inidcates reference slot in use.
C
C  Subroutine / function details:
C     DSA_WRUSER  Output message string to user.
C     ICH_LEN     Position of last non-blank char in string.
C
C  History:
C     16th June 1987.  Original version.  KS / AAO.
C     9th  Feb  1990.  Extraneous quotes removed from error message. KS/AAO
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_FIND_REF (REF_NAME,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, LENGTH, STATUS
      CHARACTER*(*) REF_NAME, OBJ_NAME
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      LOGICAL FOUND                            ! Indicates ref name in tables
      INTEGER I                                ! Loop index through tables
C
C     We look through the tables backwards, since they generally
C     fill up from the top down.
C
      FOUND=.FALSE.
      DO I=MAX_REFS,1,-1
         IF (REF_USED(I)) THEN
            IF (REF_NAMES(I).EQ.REF_NAME) THEN
               FOUND=.TRUE.
               REF_SLOT=I
               OBJ_NAME=OBJ_NAMES(I)
               LENGTH=OBJ_LEN(I)
               GO TO 340                    ! Break out of loop.
            END IF
         END IF
      END DO
  340 CONTINUE
C
      IF (.NOT.FOUND) THEN
         CALL DSA_WRUSER('Unable to find "')
         CALL DSA_WRUSER(REF_NAME(:ICH_LEN(REF_NAME)))
         CALL DSA_WRUSER(
     :                  '" in the reference name tables.  It seems it')
         CALL DSA_WRUSER(
     :           ' has not been opened.  Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOREF
      END IF
C
      END
