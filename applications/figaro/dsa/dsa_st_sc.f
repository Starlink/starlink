C+
C                     D S A _ S E T _ S T R U C T _ V A R
C
C  Routine name:
C     DSA_SET_STRUCT_VAR
C
C  Function:
C     Sets a variable used in a structure definition
C
C  Description:
C     Structure definitions read in by DSA_READ_STRUCT_DEF often have
C     variable parameters that control the details of the structure -
C     the number of elements in an array, for example.  These variables
C     may be set by this routine, which should be called before the
C     structure definition is used, but after it has been read.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SET_STRUCT_VAR (VARIABLE,VALUE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) VARIABLE      (Fixed string,descr) The name of the variable
C                       to be set.  Case is not significant.
C     (>) VALUE         (Fixed string,descr) The string to be used in
C                       place of the variable when the definition is
C                       used.
C     (!) STATUS        (Integer,ref) Status code.  If bad status is
C                       passed, this routine returns immediately.
C
C  External subroutines / functions used:
C     ICH_FOLD, ICH_LEN, DSA_WRUSER
C
C  Prior requirements:
C     The structure definition file must have been read in by
C     DSA_READ_STRUCT_DEF.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN    Position of last non-blank char in string
C     ICH_FOLD   Fold string to upper case
C     DSA_WRUSER Ouptut message to user
C
C  History:
C     31st August 1987  Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SET_STRUCT_VAR (VARIABLE,VALUE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) VARIABLE, VALUE
C
C     DSA_ structure definition common
C
      INCLUDE 'DSA_STRUCTURE'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     Local variables
C
      INTEGER   COUNT               ! Number of defined variables
      LOGICAL   FOUND               ! Indicates variable found in common
      INTEGER   I                   ! Loop index through symbol names
      INTEGER   INVOKE              ! Dummy function value
      CHARACTER SYMBOL*32           ! Upper case version of symbol name
      CHARACTER VAR_UC*32           ! Upper case version of VARIABLE
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     We will do all comparisions in upper case
C
      VAR_UC=VARIABLE
      INVOKE=ICH_FOLD(VAR_UC)
C
C     Look through the variables in the common tables, and see if
C     we can set the one specified.
C
      FOUND=.FALSE.
      DO I=1,SYMBOLS_DEFINED
         IF (SYMBOL_STATE(I).NE.EQUATE) THEN
            SYMBOL=SYMBOL_NAMES(I)
            INVOKE=ICH_FOLD(SYMBOL)
            IF (SYMBOL.EQ.VAR_UC) THEN
               FOUND=.TRUE.
               SYMBOL_VALUES(I)=VALUE
               SYMBOL_STATE(I)=DEFINED_VAR
               GO TO 320     ! Break out of I loop
            END IF
         END IF
      END DO
  320 CONTINUE
C
C     Error if not found
C
      IF (.NOT.FOUND) THEN
         CALL DSA_WRUSER('Unable to set the structure variable "')
         CALL DSA_WRUSER(VAR_UC(:ICH_LEN(VAR_UC)))
         CALL DSA_WRUSER('", since it was not defined as a variable. ')
         COUNT=0
         DO I=1,SYMBOLS_DEFINED
            IF (SYMBOL_STATE(I).NE.EQUATE) THEN
               COUNT=COUNT+1
               SYMBOL=SYMBOL_NAMES(I)
               INVOKE=ICH_FOLD(SYMBOL)
               IF (COUNT.GT.1) CALL DSA_WRUSER(', ')
               CALL DSA_WRUSER(SYMBOL(:ICH_LEN(SYMBOL)))
            END IF
         END DO
         IF (COUNT.EQ.0) THEN
            CALL DSA_WRUSER('No structure variables are')
         ELSE IF (COUNT.EQ.1) THEN
            CALL DSA_WRUSER('  is the only structure variable')
         ELSE
            CALL DSA_WRUSER(' are the only structure variables')
         END IF
         CALL DSA_WRUSER(' defined by the file ')
         CALL DSA_WRUSER(FULL_NAME(:ICH_LEN(FULL_NAME)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOSVAR
      END IF
C
      END
