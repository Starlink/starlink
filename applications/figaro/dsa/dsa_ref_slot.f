C+
C                       D S A _ R E F _ S L O T
C
C  Routine name:
C     DSA_REF_SLOT
C
C  Function:
C     Gets the slot number for a reference name in the DSA_ common tables
C
C  Description:
C     DSA_REF_SLOT looks up a reference name in the common tables
C     maintained by the DSA_ system, and returns its slot number.
C     It is similar to DSA_FIND_REF, but only returns the slot number,
C     and does not require the reference name to be in upper case.  This
C     is more in line with what is required by later versions of DSA
C     routines. This routine assumes that the reference name is in the
C     tables, and outputs an error message if this is not the case.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name.  The
C                        case is not significant.
C     (<) REF_SLOT       (Integer,ref) The slot number for the name.
C     (!) STATUS         (Integer,ref) Status code.  If a bad status value
C                        is passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ package.
C
C  External subroutines / functions used:  DSA_WRUSER, ICH_LEN, ICH_FOLD
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
C     (>) REF_NAMES   (String array) Reference names in use.
C     (>) REF_USED    (Logical array) Inidcates reference slot in use.
C
C  Subroutine / function details:
C     DSA_WRUSER  Output message string to user.
C     ICH_LEN     Position of last non-blank char in string.
C     ICH_FOLD    Convert a string into upper case.
C
C  History:
C     15th Jan 1990.  Original version.  KS / AAO.
C     12th Feb 1990.  Extraneous quotes removed from error message.  KS/AAO
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STATUS
      CHARACTER*(*) REF_NAME
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
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
      LOGICAL   FOUND                     ! Indicates ref name in tables
      INTEGER   I                         ! Loop index through tables
      INTEGER   INVOKE                    ! Ignored function value
      CHARACTER REF_NAME_UC*32            ! Upper case version of REF_NAME
C
C     Get upper case version of REF_NAME
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
C
C     We look through the tables backwards, since they generally
C     fill up from the top down.
C
      FOUND=.FALSE.
      DO I=MAX_REFS,1,-1
         IF (REF_USED(I)) THEN
            IF (REF_NAMES(I).EQ.REF_NAME_UC) THEN
               FOUND=.TRUE.
               REF_SLOT=I
               GO TO 340                    ! Break out of loop.
            END IF
         END IF
      END DO
  340 CONTINUE
C
      IF (.NOT.FOUND) THEN
         CALL DSA_WRUSER('Unable to find "')
         CALL DSA_WRUSER(REF_NAME_UC(:ICH_LEN(REF_NAME_UC)))
         CALL DSA_WRUSER(
     :                  '" in the reference name tables.  It seems it')
         CALL DSA_WRUSER(
     :           ' has not been opened.  Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOREF
      END IF
C
      END
