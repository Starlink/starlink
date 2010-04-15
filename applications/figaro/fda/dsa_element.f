      SUBROUTINE DSA_ELEMENT_NAME (REF_NAME,ELEMENT_ID,OBJECT_NAME,
     :                                                         STATUS)
*+
*  Name:
*     DSA_ELEMENT_NAME

*  Purpose:
*     Return the DTA name for an element of a defined structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_ELEMENT_NAME( DSAREF, ELEMID, OBJNAM, STATUS )

*  Description:
*     A structure definition file can equate element identifiers with
*     the name of a specific element of the structure. If such a defined
*     structure has been created and associated with a reference name,
*     this routine will return the actual DTA system object name for the
*     element corresponding to a specified element identifier within
*     that structure.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     ELEMID = CHARACTER * ( * ) (Given)
*        The element identifier.
*     OBJNAM = CHARACTER * ( * ) (Returned)
*        The DTA system name for the corresponding structure element.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     04 Sep 1987 (ks):
*        Original version.
*     18 Mar 1988 (ks):
*        Comments updated.
*     15 Jan 1990 (ks):
*        Now allows for different structure types.
*     03 Mar 1991 (ks):
*        Now allows variable names to be used in value strings in EQUATE
*        statements in definition files.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     04 Mar 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'DSA_STRUCTURE'     ! Global structure variables

*  Arguments Given:
      CHARACTER * ( * ) REF_NAME
      CHARACTER * ( * ) ELEMENT_ID

*  Arguments Returned:
      CHARACTER * ( * ) OBJECT_NAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL FOUND              ! Indicates element defined
      LOGICAL WRONG_VARIANT      ! True if defined, but for different variant
      INTEGER I                  ! Loop index through symbols
      INTEGER VARIANT            ! Variant code matching file type
      CHARACTER * ( 32 ) ELEMENT_UC ! Upper case version of ELEMENT_ID
      CHARACTER * ( 32 ) REF_NAME_UC ! Upper case version of REF_NAME
      CHARACTER * ( 32 ) SYMBOL  ! Name of defined symbol in common
      CHARACTER * ( 80 ) SYMBOL_VALUE ! Symbol value, with variables translated

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Return immediately on bad status
      IF (STATUS.NE.0) RETURN

*  We need upper case versions of REF_NAME and ELEMENT_ID
      ELEMENT_UC=ELEMENT_ID
      CALL CHR_UCASE(ELEMENT_UC)
      REF_NAME_UC=REF_NAME
      CALL CHR_UCASE(REF_NAME_UC)

*  See what file type we are looking for. DST variants are passee.
      VARIANT=NDF_TYPE
      WRONG_VARIANT=.FALSE.

*  Look through the equated symbols in the common tables, and see if
*  we can set the one specified.
      FOUND=.FALSE.
      DO I=1,SYMBOLS_DEFINED
         IF (SYMBOL_STATE(I).EQ.EQUATE) THEN
            SYMBOL=SYMBOL_NAMES(I)
            CALL CHR_UCASE(SYMBOL)
            IF (SYMBOL.EQ.ELEMENT_UC) THEN
               IF ((SYMBOL_TYPE_FLAG(I).EQ.ANY_TYPE).OR.
     :             (SYMBOL_TYPE_FLAG(I).EQ.VARIANT)) THEN
                  FOUND=.TRUE.
                  SYMBOL_VALUE=SYMBOL_VALUES(I)
                  CALL DSA3_TRNVAR(SYMBOL_VALUE,STATUS)
                  IF (STATUS.NE.0) GO TO 500
                  OBJECT_NAME=REF_NAME_UC(:CHR_LEN(REF_NAME_UC))//
     :               SYMBOL_VALUE
                  GO TO 320     ! Break out of I loop
               END IF
               WRONG_VARIANT=.TRUE.
            END IF
         END IF
      END DO
  320 CONTINUE

*  Error if not found
      IF (.NOT.FOUND) THEN
         IF (.NOT.WRONG_VARIANT) THEN
            CALL MSG_SETC('FDA_T019','The symbol '//
     :         ELEMENT_UC(:CHR_LEN(ELEMENT_UC))//
     :         ' has not been defined.')
         ELSE
            CALL MSG_SETC('FDA_T019','The symbol '//
     :         ELEMENT_UC(:CHR_LEN(ELEMENT_UC))//
     :         ' has not been defined.'//
     :         '(Although it is defined For DST format files.)')
         END IF
         CALL ERR_MARK
            STATUS=SAI__ERROR
            CALL ERR_REP('FDA_E066','DSA_ELEMENT_NAME: ^FDA_T019',
     :         STATUS)
            CALL ERR_FLUSH(STATUS)
         CALL ERR_RLSE
         STATUS=1
      END IF

*  Exit

  500 CONTINUE

      END



      SUBROUTINE DSA3_TRNVAR (STRING,STATUS)
C+
C                    D S A 3 _ T R N V A R
C
C  Routine name:
C     DSA3_TRNVAR
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
C-
C  Common variable details:
C     (>) EQUATE         (Integer parameter) Flags symbol as an EQUATE
C     (>) SYMBOLS_DEFINED(Integer) Number of defined symbols
C     (>) SYMBOL_NAMES   (String array) Names of defined symbols
C     (>) SYMBOL_STATE   (Integer array) States of symbols
C     (>) SYMBOL_VALUES  (String array) Values of defined symbols
C
C  History:
C     2nd Sept 1987   Original version.  KS / AAO.
C     18th March 1988 Comments modified. KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     4th Mar 1996    FDA library. HME/UoE,Starlink
C     11th Mar 1996   Fix bug whereby LWORD=0 as returned by DSA3_WORD
C                     might still be used as upper limit of substring.
C     30th Jul 1996   WSTRING for catenations. MJCL / Starlink, UCL.
C+
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) STRING
C
C     Functions used
C
      INTEGER CHR_LEN, DSA3_WORD
C
C     DSA_ system structure definition common
C
      INCLUDE 'DSA_STRUCTURE'
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
      CHARACTER WSTRING*255  ! Workspace for catenation
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
         POSN=DSA3_WORD(STRING,IST,'.,[] ;-+','{}''''""',WORD,LWORD,CHR)
         TRANS=.FALSE.
         DO I=1,SYMBOLS_DEFINED
            IF (SYMBOL_STATE(I).NE.EQUATE) THEN
               IF (LWORD.GT.0) THEN
                  IF (WORD(:LWORD).EQ.SYMBOL_NAMES(I)) THEN
                     IF (SYMBOL_STATE(I).NE.DEFINED_VAR) THEN
                        CALL ERR_MARK
                        STATUS=SAI__ERROR
                        WSTRING = 'DSA3_TRNVAR: '//
     :                     'The structure definition variable '//
     :                     WORD(:LWORD)//' is undefined. The string "'//
     :                     STRING(:CHR_LEN(STRING))//
     :                     '" cannot be properly translated.'
                        CALL ERR_REP('FDA_E067', WSTRING, STATUS)
                        CALL ERR_FLUSH(STATUS)
                        CALL ERR_RLSE
                        STATUS=1
                        GO TO 500
                     END IF
                     IST=INDEX(STRING,WORD(:LWORD))
                     LSUB=CHR_LEN(SYMBOL_VALUES(I))
                     WORK_STRING=
     :                    SYMBOL_VALUES(I)(:LSUB)//STRING(IST+LWORD:)
                     STRING(IST:)=WORK_STRING
                     TRANS=.TRUE.
                     GO TO 320           ! Break symbol search loop
                  END IF
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
