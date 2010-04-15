C+
C                  D S A _ D E F _ S T R U C T _ T Y P E
C
C  Routine name:
C     DSA_DEF_STRUCT_TYPE
C
C  Function:
C     Gets the type of a structure defined by a structure definition file
C
C  Description:
C     If a set of structure definitions has been read in by DSA_READ_STRUCT_
C     DEF, this routine will determine the actual type corresponding to a
C     specific structure identifier.  It also acts as a test to see if the
C     type has in fact been defined.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DEF_STRUCT_TYPE (REF_SLOT,STRUCTURE_ID,MUST_EXIST,TYPE,
C                                                    STRUCT_NO,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT        (Integer,ref) The common slot number used for
C                         the structure in which the structure is being
C                         built. This is used to determine the format
C                         being used.
C     (>) STRUCTURE_ID    (Fixed string,descr) The name used to
C                         refer to the structure in the definition file.
C     (>) MUST_EXIST      (Logical, ref) If true, the structure must have
C                         been defined and an error is reported if it does
C                         not.  If false, no error messages are output and
C                         non-existence is signalled just by setting
C                         STRUCT_NO to zero.
C     (<) TYPE            (Fixed string,descr) The structure type.  This
C                         is a type that can be passed to a DTA_ routine.
C     (<) STRUCT_NO       (Integer,ref) The number of the entry in the
C                         common tables that refers to this structure.
C     (!) STATUS          (Integer,ref) Status code.  If bad status is passed
C                         to this routine, it returns immediately.
C
C  External subroutines / functions used:
C     ICH_LEN, ICH_FOLD, DSA_WRUSER
C
C  Prior requirements:
C     DSA_READ_STRUCT_DEF should have been called to process the
C     structure definition file.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN       Position of last non-blank char in string
C     ICH_FOLD      Fold string to upper case
C     DSA_WRUSER    Output string to user
C
C  History:
C     1st Sept 1987   Original version.  KS / AAO.
C     15th Jan 1990   Modified to handle different formats.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_DEF_STRUCT_TYPE (REF_SLOT,STRUCTURE_ID,MUST_EXIST,
     :                                          TYPE,STRUCT_NO,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL MUST_EXIST
      INTEGER REF_SLOT,STRUCT_NO, STATUS
      CHARACTER*(*) STRUCTURE_ID, TYPE
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ structure definition common
C
      INCLUDE 'DSA_STRUCTURE'
C
C     Local variables
C
      INTEGER   COUNT               ! Number of defined structures
      INTEGER   I                   ! Loop index through structures
      INTEGER   INVOKE              ! Dummy function argument
      CHARACTER STRUCT_UC*32        ! Upper case version of STRUCTURE_ID
      INTEGER   VARIANT             ! Variant code matching file type
      LOGICAL   WRONG_VARIANT       ! True if defined, but for different variant
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Make sure we have an upper case version of STRUCTURE_ID
C
      STRUCT_UC=STRUCTURE_ID
      INVOKE=ICH_FOLD(STRUCT_UC)
C
C     See what file type we are looking for.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
         VARIANT=NDF_TYPE
      ELSE
         VARIANT=DST_TYPE
      END IF
      WRONG_VARIANT=.FALSE.
C
C     Now search through the defined structure names, looking
C     for one that matches.
C
      STRUCT_NO=0
      DO I=1,STRUCTS_DEFINED
         IF (STRUCT_UC.EQ.STRUCT_NAMES(I)) THEN
            IF ((STRUCT_TYPE_FLAG(I).EQ.ANY_TYPE).OR.
     :           (STRUCT_TYPE_FLAG(I).EQ.VARIANT)) THEN
               STRUCT_NO=I
               TYPE=STRUCT_TYPE(I)
               GO TO 320     ! Break out of I loop
            END IF
            WRONG_VARIANT=.TRUE.
         END IF
      END DO
  320 CONTINUE
C
      IF ((STRUCT_NO.EQ.0).AND.MUST_EXIST) THEN
         CALL DSA_WRUSER('The structure identifier "')
         CALL DSA_WRUSER(STRUCT_UC(:ICH_LEN(STRUCT_UC)))
         CALL DSA_WRUSER('" has not been defined. ')
         IF (WRONG_VARIANT) THEN
            CALL DSA_WRUSER ('(Although it is defined for ')
            IF (NDF_FORMAT(REF_SLOT)) THEN
               CALL DSA_WRUSER('DST')
            ELSE
               CALL DSA_WRUSER('NDF')
            END IF
            CALL DSA_WRUSER(' format files.) ')
         END IF
         COUNT=0
         DO I=1,STRUCTS_DEFINED
            COUNT=COUNT+1
            IF (COUNT.GT.1) CALL DSA_WRUSER(', ')
            CALL DSA_WRUSER(STRUCT_NAMES(I)(:ICH_LEN(STRUCT_NAMES(I))))
            IF (STRUCT_TYPE_FLAG(I).EQ.NDF_TYPE) THEN
               CALL DSA_WRUSER(' (NDF)')
            ELSE IF (STRUCT_TYPE_FLAG(I).EQ.DST_TYPE) THEN
               CALL DSA_WRUSER(' (DST)')
            END IF
         END DO
         IF (COUNT.EQ.0) THEN
            CALL DSA_WRUSER('No structures are')
         ELSE IF (COUNT.EQ.1) THEN
            CALL DSA_WRUSER('  is the only structure')
         ELSE
            CALL DSA_WRUSER(' are the only structures')
         END IF
         CALL DSA_WRUSER(' defined by the file ')
         CALL DSA_WRUSER(FULL_NAME(:ICH_LEN(FULL_NAME)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOSTRU
      END IF
C
      END
