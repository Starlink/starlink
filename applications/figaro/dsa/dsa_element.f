C+
C
C                    D S A _ E L E M E N T _ N A M E
C
C  Routine name:
C     DSA_ELEMENT_NAME
C
C  Function:
C     Returns the DTA_ name for an element of a defined structure
C
C  Description:
C     A structure definition file can equate element identifiers with
C     the name of a specific element of the structure.  If such a
C     defined structure has been created and associated with a reference
C     name, this routine will return the actual DTA_ system object name
C     for the element corresponding to a specified element identifier
C     within that structure.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ELEMENT_NAME (REF_NAME,ELEMENT_ID,OBJECT_NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name
C                       associated with the structure.
C     (>) ELEMENT_ID    (Fixed string,descr) The element identifier.
C     (<) OBJECT_NAME   (Fixed string,descr) The DTA_ system name for
C                       the corresponding structure element.
C     (!) STATUS        (Integer,ref) Status value.  If bad status is
C                       passed to this routine, it returns immediately.
C
C  External variables used:
C     Common variables internal to the DSA package
C
C  External subroutines / functions used:
C     ICH_FOLD, ICH_LEN, DSA_FIND_REF, DSA_TRANSLATE_VAR, DSA_WRUSER
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system.
C     The structure definition file that defines the structure must
C     have been processed by DSA_READ_STRUCT_DEF, and the structure
C     must have been opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) ANY_TYPE       (Integer parameter) Flags not limited to any variant
C     (>) DST_TYPE       (Integer parameter) Indicates limited to DST variants
C     (>) EQUATE         (Integer parameter) Flags symbol as an EQUATE
C     (>) NDF_TYPE       (Integer parameter) Indicates limited to NDF variants
C     (>) SYMBOLS_DEFINED(Integer) Number of defined symbols
C     (>) SYMBOL_NAMES   (String array) Names of defined symbols
C     (>) SYMBOL_STATE   (Integer array) States of symbols
C     (>) SYMBOL_TYPE_FLAG(Integer array) Flags symbol defined for given type
C     (>) SYMBOL_VALUES  (String array) Values of defined symbols
C     (>) FULL_NAME      (Character) Full name of definition file
C
C  Subroutine / function details:
C     ICH_FOLD      Convert string to upper case
C     ICH_LEN       Position of last non-blank char in string
C     DSA_FIND_REF  Look up reference name in internal tables
C     DSA_WRUSER    Output string to user
C     DSA_TRANSLATE_VAR Translate structure variables occurring in a string
C
C  History:
C     4th Sept 1987   Original version.  KS / AAO.
C     18th March 1988 Comments updated.  KS / AAO.
C     15th Jan  1990  Now allows for different structure types. KS/AAO.
C     3rd March 1991  Now allows variable names to be used in value strings
C                     in EQUATE statements in definition files. KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_ELEMENT_NAME (REF_NAME,ELEMENT_ID,OBJECT_NAME,
     :                                                         STATUS)
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME, ELEMENT_ID, OBJECT_NAME
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system structure definition common
C
      INCLUDE 'DSA_STRUCTURE'
C
C     Local variables
C
      INTEGER   COUNT              ! Number of equated symbols in tables
      CHARACTER ELEMENT_UC*32      ! Upper case version of ELEMENT_ID
      LOGICAL   FOUND              ! Indicates element defined
      INTEGER   I                  ! Loop index through symbols
      INTEGER   INVOKE             ! Dummy function value
      INTEGER   LENGTH             ! Significant chars in TOP_LEVEL_NAME
      CHARACTER TOP_LEVEL_NAME*32  ! Top level name associated with REF_NAME
      CHARACTER REF_NAME_UC*32     ! Upper case version of REF_NAME
      INTEGER   REF_SLOT           ! Common slot for REF_NAME - ignored
      CHARACTER SYMBOL*32          ! Name of defined symbol in common
      CHARACTER SYMBOL_VALUE*80    ! Symbol value, with variables translated
      INTEGER   VARIANT            ! Variant code matching file type
      LOGICAL   WRONG_VARIANT      ! True if defined, but for different variant
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     We need upper case versions of REF_NAME and ELEMENT_ID
C
      ELEMENT_UC=ELEMENT_ID
      INVOKE=ICH_FOLD(ELEMENT_UC)
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
C
C     Look up REF_NAME in common tables
C
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,TOP_LEVEL_NAME,LENGTH,
     :                                                        STATUS)
      IF (STATUS.NE.0) GO TO 500      ! Error exit
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
C     Look through the equated symbols in the common tables, and see if
C     we can set the one specified.
C
      FOUND=.FALSE.
      DO I=1,SYMBOLS_DEFINED
         IF (SYMBOL_STATE(I).EQ.EQUATE) THEN
            SYMBOL=SYMBOL_NAMES(I)
            INVOKE=ICH_FOLD(SYMBOL)
            IF (SYMBOL.EQ.ELEMENT_UC) THEN
               IF ((SYMBOL_TYPE_FLAG(I).EQ.ANY_TYPE).OR.
     :             (SYMBOL_TYPE_FLAG(I).EQ.VARIANT)) THEN
                  FOUND=.TRUE.
                  SYMBOL_VALUE=SYMBOL_VALUES(I)
                  CALL DSA_TRANSLATE_VAR (SYMBOL_VALUE,STATUS)
                  IF (STATUS.NE.0) GO TO 500   ! Error exit
                  OBJECT_NAME=TOP_LEVEL_NAME(:LENGTH)//SYMBOL_VALUE
                  GO TO 320     ! Break out of I loop
               END IF
               WRONG_VARIANT=.TRUE.
            END IF
         END IF
      END DO
  320 CONTINUE
C
C     Error if not found
C
      IF (.NOT.FOUND) THEN
         CALL DSA_WRUSER('The symbol "')
         CALL DSA_WRUSER(ELEMENT_UC(:ICH_LEN(ELEMENT_UC)))
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
         DO I=1,SYMBOLS_DEFINED
            IF (SYMBOL_STATE(I).EQ.EQUATE) THEN
               COUNT=COUNT+1
               SYMBOL=SYMBOL_NAMES(I)
               INVOKE=ICH_FOLD(SYMBOL)
               IF (COUNT.GT.1) CALL DSA_WRUSER(', ')
               CALL DSA_WRUSER(SYMBOL(:ICH_LEN(SYMBOL)))
               IF (SYMBOL_TYPE_FLAG(I).EQ.NDF_TYPE) THEN
                  CALL DSA_WRUSER(' (NDF)')
               ELSE IF (SYMBOL_TYPE_FLAG(I).EQ.DST_TYPE) THEN
                  CALL DSA_WRUSER(' (DST)')
               END IF
            END IF
         END DO
         IF (COUNT.EQ.0) THEN
            CALL DSA_WRUSER('No symbols are')
         ELSE IF (COUNT.EQ.1) THEN
            CALL DSA_WRUSER('  is the only symbol')
         ELSE
            CALL DSA_WRUSER(' are the only symbols')
         END IF
         CALL DSA_WRUSER(' equated to elements in the file ')
         CALL DSA_WRUSER(FULL_NAME(:ICH_LEN(FULL_NAME)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOSVAR
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
