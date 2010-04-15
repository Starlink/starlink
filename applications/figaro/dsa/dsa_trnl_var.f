C+
C                    D S A _ T R A N S L A T E _ V A R
C
C  Routine name:
C     DSA_TRANSLATE_VAR
C
C  Function:
C     Translates any structure variables in a string
C
C  Description:
C     This routine takes as input a string that may contain occurrences
C     of one or more of the structure definition variables processed
C     by DSA_READ_STRUCT_DEF.  The string is returned with all of these
C     variables translated (or with an error if a variable has not been
C     defined.)
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_TRANSLATE_VAR (STRING,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (!) STRING      (Fixed string,descr) The string in question
C                     (>) The string, including variables
C                     (<) The string with variables substituted
C     (!) STATUS      (Integer,ref) Status code.  This routine returns
C                     immediately if bad status is passed.
C
C  External variables used:
C     Common variables internal to the DSA_ routines
C
C  External subroutines / functions used:
C     ICH_LEN, ICH_WORD, DSA_WRUSER
C
C  Prior requirements:
C     DSA_READ_STRUCT_DEF must have processed the structure definition
C     file declaring the variables.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) EQUATE         (Integer parameter) Flags symbol as an EQUATE
C     (>) SYMBOLS_DEFINED(Integer) Number of defined symbols
C     (>) SYMBOL_NAMES   (String array) Names of defined symbols
C     (>) SYMBOL_STATE   (Integer array) States of symbols
C     (>) SYMBOL_VALUES  (String array) Values of defined symbols
C
C  Subroutine / function details:
C
C  History:
C     2nd Sept 1987   Original version.  KS / AAO.
C     18th March 1988 Comments modified. KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_TRANSLATE_VAR (STRING,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) STRING
C
C     Functions used
C
      INTEGER ICH_LEN, ICH_WORD
C
C     DSA_ system structure definition common
C
      INCLUDE 'DSA_STRUCTURE'
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER CHR*1        ! Delimiting character - ignored
      INTEGER   I            ! Loop index through variables
      INTEGER   IST          ! Start of word being delimited/translated
      INTEGER   LSUB         ! Length of substitute string
      INTEGER   LWORD        ! Length of delimited word
      INTEGER   POSN         ! Position of delimiting character
      LOGICAL   TRANS        ! Indicates a translation was performed
      CHARACTER WORD*64      ! Word delimited from string
      CHARACTER WORK_STRING*80 ! Temporary string for translation
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pass through the string, delimiting words that may be variables,
C     looking them up, substituting them if so.  Note that variables
C     are case sensitive.
C
      IST=1
      POSN=1
      DO WHILE (POSN.GT.0)
         POSN=ICH_WORD(STRING,IST,'.,[] ;-+','{}''''""',WORD,LWORD,CHR)
         TRANS=.FALSE.
         DO I=1,SYMBOLS_DEFINED
            IF (SYMBOL_STATE(I).NE.EQUATE) THEN
               IF (WORD(:LWORD).EQ.SYMBOL_NAMES(I)) THEN
                  IF (SYMBOL_STATE(I).NE.DEFINED_VAR) THEN
                     CALL DSA_WRUSER(
     :                          'The structure definition variable "')
                     CALL DSA_WRUSER(WORD(:LWORD))
                     CALL DSA_WRUSER('" is undefined.  The string "')
                     CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
                     CALL DSA_WRUSER(
     :                            '" cannot be properly translated.')
                     CALL DSA_WRFLUSH
                     STATUS=DSA__BLDERR
                     GO TO 500        ! Error exit
                  END IF
                  IST=INDEX(STRING,WORD(:LWORD))
                  LSUB=ICH_LEN(SYMBOL_VALUES(I))
                  WORK_STRING=
     :                 SYMBOL_VALUES(I)(:LSUB)//STRING(IST+LWORD:)
                  STRING(IST:)=WORK_STRING
                  TRANS=.TRUE.
                  GO TO 320           ! Break symbol search loop
               END IF
            END IF
         END DO
  320    CONTINUE                     ! End of symbol search loop
         IF (.NOT.TRANS) IST=POSN+1
      END DO
C
C     Exit
C
  500 CONTINUE
C
      END
