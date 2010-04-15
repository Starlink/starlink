C+
C                   D S A _ R E A D _ S T R U C T _ D E F
C
C  Routine name:
C     DSA_READ_STRUCT_DEF
C
C  Function:
C     Reads one or more parameterised structure definitions from a file.
C
C  Description:
C     This routine reads a specified text file containing one or more
C     parameterised structure definitions.  The values of the parameters
C     may be set later by calls to DSA_SET_PARM, and structures
C     corresponding to the definitions may then be created through calls
C     to routines such as DSA_CREATE_STRUCTURE.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_READ_STRUCT_DEF (FILENAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) FILENAME      (Fixed string,descr) The name of the definition
C                       file to be used.  The default extension is '.DEF'
C                       and the usual Figaro directory search path will
C                       be followed in looking for it.
C     (!) STATUS        (Integer,ref) Status code.  If bad status is
C                       passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ routines.
C
C  External subroutines / functions used:
C     ICH_FOLD, ICH_LEN, ICH_WORD, ICH_VERIF, ICH_CI, DSA_DEF_ERROR,
C     DSA_OPEN_TEXT_FILE, DSA_FREE_LU, DSA_WRUSER, GEN_FORTERR, GEN_DETAB
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  External variable details:
C     (>) ANY_TYPE       (Integer parameter) Flags not limited to any variant
C     (>) DEFINED_VAR    (Integer parameter) Flags symbol as defined VARIABLE
C     (>) DST_TYPE       (Integer parameter) Indicates limited to DST variants
C     (<) ELEMENT_COND   (String array) Condition for including element
C     (<) ELEMENT_NAME   (String array) Names of structure elements
C     (<) ELEMENT_TYPE   (String array) Types of structure elements
C     (>) EQUATE         (Integer parameter) Flags symbol as an EQUATE
C     (<) FULL_NAME      (Character) Full name of definition file
C     (>) MAX_STRUCT_LINES (Integer parameter) Max no. of definition lines
C     (>) MAX_STRUCTS    (Integer parameter) Max number of structures
C     (>) MAX_SYMBOLS    (Integer parameter) Maximum number of symbols
C     (>) NDF_TYPE       (Integer parameter) Indicates limited to NDF variants
C     (<) STRUCTS_DEFINED(Integer) Number of defined structures
C     (<) STRUCT_NAMES   (String array) Names of defined structures
C     (<) STRUCT_TYPE    (String array) Types of defined structures
C     (<) STRUCT_TYPE_FLAG(Integer array) Flags structure defined for given type
C     (<) STRUCT_START   (Integer array) First element of structure
C     (<) STRUCT_END     (Integer array) Last element of structure
C     (<) SYMBOLS_DEFINED(Integer) Number of defined symbols
C     (<) SYMBOL_NAMES   (String array) Names of defined symbols
C     (<) SYMBOL_STATE   (Integer array) States of symbols
C     (<) SYMBOL_TYPE_FLAG(Integer array) Flags symbol defined for given type
C     (<) SYMBOL_VALUES  (String array) Values of defined symbols
C     (>) UNDEFINED_VAR  (Integer parameter) Flags symbol as undefined
C
C  Subroutine / function details:
C     ICH_FOLD    Fold string to upper case
C     ICH_LEN     Position of last non blank char in string
C     ICH_WORD    Delimits next word in string
C     ICH_VERIF   Next char in string not in specified set
C     ICH_CI      Formats an integer  (used by DSA_DEF_ERROR)
C     DSA_WRUSER  Output string to user
C     DSA_DEF_ERROR  Utility subroutine for this routine - outputs error message
C     DSA_FREE_LU Release a logical unit number and close any file open on it
C     DSA_OPEN_TEXT_FILE Opens a text file
C     GEN_DETAB   Removes TAB characters from a string
C     GEN_FORTERR Get text for Fortran I/O error
C
C  History:
C     28th Aug 1987.  Original version.  KS / AAO.
C     18th Mar 1987.  Comments completed.  KS / AAO.
C     15th Jan 1990.  Now supports variant structure types. KS/AAO.
C     3rd  Mar 1991.  Now allows TAB characters in structure definitions.
C                     Uses DSA_OPEN_TEXT_FILE instead of LIB$GET/FREE_LUN
C                     and FIG_OPFILE to open the file. KS/AAO.
C  Note:
C     This routine is extremely crude.  In particular, the way "IF" clauses
C     are handled (the fact that they cannot be nested, for example) is
C     a limitation.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     3rd  Sep 1992   Changed default extension to lowercase. HME/UoE
C+
      SUBROUTINE DSA_READ_STRUCT_DEF (FILENAME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) FILENAME
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN, ICH_WORD, ICH_VERIF
C
C     DSA_ structure definition common
C
      INCLUDE 'DSA_STRUCTURE'
C
C     DSA_ common variables
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER CHR*1              ! Used for any single character
      LOGICAL   COND_DEF           ! Indicates in an "IF" clause
      CHARACTER COND_VARIABLE*32   ! Test condition for IF clause
      CHARACTER DEFAULT*64         ! Default value for variable
      LOGICAL   EOF                ! Indicates end of definition file
      CHARACTER ERROR*64           ! Error description string
      INTEGER   FILE_LINE          ! Line number in definition file
      INTEGER   FIRST              ! First non-blank char posn in line
      INTEGER   FLEN               ! Length of full file name
      LOGICAL   FOPEN              ! Indicates file opened
      INTEGER   FOR_STATUS         ! Fortran I/O system status
      INTEGER   I                  ! General loop variable
      INTEGER   IGNORE             ! Dummy status argument
      CHARACTER INPUT_LINE*128     ! Line (possibly with TABS) read from file
      INTEGER   INVOKE             ! Dummy function argument
      CHARACTER LINE*128           ! Line read from file - after detabbing
      INTEGER   LU                 ! Logical unit number
      INTEGER   LWORD              ! Length of word
      CHARACTER NAME*64            ! Symbol name in file
      INTEGER   POSN               ! Scan pointer through record
      LOGICAL   STRUCT_DEF         ! Indicates in structure definition
      INTEGER   STRUCT_LINE        ! Last used structure line entry
      INTEGER   TABS(16)           ! TAB positions
      CHARACTER TYPE*32            ! Type of structure element
      CHARACTER UWORD*64           ! Upper case version of WORD
      INTEGER   VARIANT            ! Current variant, if any
      CHARACTER WORD*64            ! Word delimited from input line
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Some initial values
C
      FOPEN=.FALSE.
      SYMBOLS_DEFINED=0
      STRUCTS_DEFINED=0
C
C     Set standard 8 column TAB positions
C
      DO I=1,16
         TABS(I)=I*8+1
      END DO
C
C     Try to open the file
C
      CALL DSA_OPEN_TEXT_FILE(FILENAME,'.def','OLD',.FALSE.,LU,
     :                                               FULL_NAME,STATUS)
      IF (STATUS.NE.0) GO TO 500   ! Error exit
      FOPEN=.TRUE.
      FLEN=ICH_LEN(FULL_NAME)
C
C     Start to read through the definition file
C
      FILE_LINE=0
      STRUCT_LINE=0
      STRUCT_DEF=.FALSE.
      COND_DEF=.FALSE.
      VARIANT=ANY_TYPE
      EOF=.FALSE.
      DO WHILE (.NOT.EOF)
         READ (LU,'(A)',IOSTAT=FOR_STATUS) INPUT_LINE
         IF (FOR_STATUS.LT.0) THEN
            EOF=.TRUE.
         ELSE IF (FOR_STATUS.NE.0) THEN
            CALL DSA_WRUSER('I/O error while reading the structure '//
     :                                              'definition file ')
            CALL DSA_WRUSER(FULL_NAME(:FLEN))
            CALL DSA_WRUSER('. ')
            CALL GEN_FORTERR(FOR_STATUS,.FALSE.,ERROR)
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('.')
            CALL DSA_WRFLUSH
            FOR_CODE=FOR_STATUS
            IF (STATUS.NE.0) STATUS=DSA__FORERR
            GO TO 500             ! Error exit
         ELSE
C
C           We have read a line OK from the file.  Ignore it if it is
C           blank or a comment, after first removing any tabs it may have.
C
            CALL GEN_DETAB (INPUT_LINE,LINE,16,TABS)
            FILE_LINE=FILE_LINE+1
            FIRST=MAX(1,ICH_VERIF(LINE,1,' '))
            CHR=LINE(FIRST:FIRST)
            IF ((CHR.NE.' ').AND.(CHR.NE.'*')) THEN
C
C              It isn't a comment.  So, it ought to start with one of
C              the keywords (VARIANT,VARIABLE,STRUCTURE,END,IF or EQUATE)
C              or be a structure element definition, in which case it begins
C              with a '.'.  Note that the parsing performed by this
C              routine is rather 'brute force' in nature, and isn't
C              any sort of fancy parsing scheme - the allowed syntax is
C              sufficiently simple that we just about get away with this.
C
               POSN=ICH_WORD(LINE,FIRST,' ',' ',WORD,LWORD,CHR)
               UWORD=WORD
               INVOKE=ICH_FOLD(UWORD)
               IF (UWORD(:LWORD).EQ.'STRUCTURE') THEN
C
C                 'STRUCTURE' - start of definition.
C
C                 Line syntax is:   STRUCTURE name [type]
C
                  IF (STRUCT_DEF) THEN
                     CALL DSA_DEF_ERROR(
     :                   'Invalid nesting of structure definitions',
     :                              FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ',' ',NAME,LWORD,CHR)
                  ELSE
                     NAME=' '
                  END IF
                  IF (NAME.EQ.' ') THEN
                     CALL DSA_DEF_ERROR('No name given for structure',
     :                                FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ',' ',WORD,LWORD,CHR)
                  ELSE
                     WORD=' '
                  END IF
                  IF (STRUCTS_DEFINED.GE.MAX_STRUCTS) THEN
                     CALL DSA_DEF_ERROR('Too many structures defined',
     :                                FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  STRUCTS_DEFINED=STRUCTS_DEFINED+1
                  STRUCT_DEF=.TRUE.
                  INVOKE=ICH_FOLD(NAME)
                  STRUCT_NAMES(STRUCTS_DEFINED)=NAME
                  STRUCT_START(STRUCTS_DEFINED)=STRUCT_LINE+1
                  STRUCT_TYPE(STRUCTS_DEFINED)=WORD
                  STRUCT_TYPE_FLAG(STRUCTS_DEFINED)=VARIANT
               ELSE IF (UWORD.EQ.'VARIABLE') THEN
C
C                 'VARIABLE' - declaration
C
C                 Line syntax is:  VARIABLE name [DEFAULT value]
C
                  IF (SYMBOLS_DEFINED.GE.MAX_SYMBOLS) THEN
                     CALL DSA_DEF_ERROR(
     :                  'Too many variables and/or equates defined',
     :                                  FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  SYMBOLS_DEFINED=SYMBOLS_DEFINED+1
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ',' ',NAME,LWORD,CHR)
                  ELSE
                     NAME=' '
                  END IF
                  IF (NAME.EQ.' ') THEN
                     CALL DSA_DEF_ERROR('No name given for variable',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  SYMBOL_NAMES(SYMBOLS_DEFINED)=NAME
                  SYMBOL_STATE(SYMBOLS_DEFINED)=UNDEFINED_VAR
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ',' ',WORD,LWORD,CHR)
                  ELSE
                     WORD=' '
                  END IF
                  DEFAULT=' '
                  IF (WORD.NE.' ') THEN
                     UWORD=WORD
                     INVOKE=ICH_FOLD(UWORD)
                     IF (UWORD(:LWORD).NE.'DEFAULT') THEN
                        CALL DSA_DEF_ERROR(
     :                        'Unrecognised word encountered where '//
     :                                        '"DEFAULT" was expected',
     :                                  FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                     END IF
                     IF (POSN.GT.0) THEN
                        POSN=ICH_WORD(LINE,POSN+1,' ','""',DEFAULT,
     :                                                       LWORD,CHR)
                     ELSE
                        DEFAULT=' '
                        CHR=' '
                     END IF
                     IF ((DEFAULT.EQ.' ').AND.(CHR.NE.'"')) THEN
                        CALL DSA_DEF_ERROR('Nothing follows "DEFAULT"',
     :                                  FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                     END IF
                     SYMBOL_VALUES(SYMBOLS_DEFINED)=DEFAULT
                     SYMBOL_STATE(SYMBOLS_DEFINED)=DEFINED_VAR
                  END IF
               ELSE IF (UWORD.EQ.'VARIANT') THEN
C
C                 'VARIANT' - declaration
C
C                 Line syntax is:  VARIANT type
C
                  IF (VARIANT.NE.ANY_TYPE) THEN
                     CALL DSA_DEF_ERROR(
     :                  'Attempt to nest VARIANT specifications.',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (STRUCT_DEF) THEN
                     CALL DSA_DEF_ERROR(
     :                   'Attempt to change variant within structure',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ',' ',NAME,LWORD,CHR)
                  ELSE
                     NAME=' '
                  END IF
                  IF (NAME.EQ.' ') THEN
                     CALL DSA_DEF_ERROR('No name given for variant',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  INVOKE=ICH_FOLD(NAME)
                  IF (NAME.EQ.'NDF') THEN
                     VARIANT=NDF_TYPE
                  ELSE IF (NAME.EQ.'DST') THEN
                     VARIANT=DST_TYPE
                  ELSE
                     CALL DSA_DEF_ERROR('Variant must be DST or NDF',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
               ELSE IF (UWORD.EQ.'EQUATE') THEN
C
C                 'EQUATE' - declaration
C
C                 Line syntax is:  EQUATE name object
C
                  IF (SYMBOLS_DEFINED.GE.MAX_SYMBOLS) THEN
                     CALL DSA_DEF_ERROR(
     :                  'Too many equates and/or variables declared',
     :                                  FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  SYMBOLS_DEFINED=SYMBOLS_DEFINED+1
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ',' ',NAME,LWORD,CHR)
                  ELSE
                     NAME=' '
                  END IF
                  IF (NAME.EQ.' ') THEN
                     CALL DSA_DEF_ERROR('No name given for equate',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  SYMBOL_NAMES(SYMBOLS_DEFINED)=NAME
                  SYMBOL_TYPE_FLAG(SYMBOLS_DEFINED)=VARIANT
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ',' ',WORD,LWORD,CHR)
                  ELSE
                     WORD=' '
                  END IF
                  IF (WORD.EQ.' ') THEN
                     CALL DSA_DEF_ERROR('No value given for equate',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  SYMBOL_VALUES(SYMBOLS_DEFINED)=WORD
                  SYMBOL_STATE(SYMBOLS_DEFINED)=EQUATE
               ELSE IF (UWORD.EQ.'IF') THEN
C
C                 'IF' - start of condition
C
C                 Line syntax is:  IF variable [THEN]
C
                  IF (.NOT.STRUCT_DEF) THEN
                     CALL DSA_DEF_ERROR('"IF" clause appears '//
     :                                 'outside structure definition',
     :                                 FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (COND_DEF) THEN
                     CALL DSA_DEF_ERROR(
     :                   'Unfortunately, "IF" clauses cannot be nested',
     :                                 FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ','() ',NAME,LWORD,CHR)
                  ELSE
                     NAME=' '
                  END IF
                  IF (NAME.EQ.' ') THEN
                     CALL DSA_DEF_ERROR('No condition specified',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  COND_VARIABLE=NAME
                  COND_DEF=.TRUE.
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ',' ',WORD,LWORD,CHR)
                     INVOKE=ICH_FOLD(WORD)
                  ELSE
                     WORD=' '
                  END IF
                  IF ((WORD.NE.' ').AND.(WORD.NE.'THEN')) THEN
                     CALL DSA_DEF_ERROR(
     :                   'Unrecognised word where "THEN" was expected',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
               ELSE IF (UWORD(:LWORD).EQ.'END') THEN
C
C                 'END' - end of definition.
C
C                 Line syntax is:   END STRUCTURE or END IF or END VARIANT
C
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ',' ',NAME,LWORD,CHR)
                     INVOKE=ICH_FOLD(NAME)
                  ELSE
                     NAME=' '
                  END IF
                  IF ((NAME.NE.' ').AND.(NAME.NE.'STRUCTURE').AND.
     :                (NAME.NE.'VARIANT').AND.(NAME.NE.'IF')) THEN
                     CALL DSA_DEF_ERROR('Invalid "END"',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (NAME.EQ.'VARIANT') THEN
                     IF (STRUCT_DEF) THEN
                        CALL DSA_DEF_ERROR('Cannot change variant '//
     :                               'within a structure definition',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                        GO TO 320      ! Abort line processing
                     END IF
                     VARIANT=ANY_TYPE
                  ELSE
                     IF (.NOT.STRUCT_DEF) THEN
                        CALL DSA_DEF_ERROR(
     :                   'Invalid nesting of structure definitions',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                        GO TO 320      ! Abort line processing
                     END IF
                     IF (COND_DEF) THEN
                        IF (NAME.EQ.'STRUCTURE') THEN
                           CALL DSA_DEF_ERROR(
     :                          'Structure ended with "IF" unclosed',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                           GO TO 320      ! Abort line processing
                        END IF
                        COND_DEF=.FALSE.
                        COND_VARIABLE=' '
                     ELSE
                        IF (NAME.EQ.'IF') THEN
                           CALL DSA_DEF_ERROR(
     :                                '"END IF" with no "IF" to end',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                           GO TO 320      ! Abort line processing
                        END IF
                        STRUCT_DEF=.FALSE.
                        STRUCT_END(STRUCTS_DEFINED)=STRUCT_LINE
                     END IF
                  END IF
               ELSE IF (UWORD(1:1).EQ.'.') THEN
C
C                 Structure element definition - starts with '.'
C
C                 Line syntax is: .element type
C
                  IF (.NOT.STRUCT_DEF) THEN
                     CALL DSA_DEF_ERROR('Structure element appears '//
     :                                 'outside structure definition',
     :                                 FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (POSN.GT.0) THEN
                     POSN=ICH_WORD(LINE,POSN+1,' ',' ',TYPE,LWORD,CHR)
                  ELSE
                     TYPE=' '
                  END IF
                  IF (TYPE.EQ.' ') THEN
                     CALL DSA_DEF_ERROR(
     :                    'No type specified for structure element',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (STRUCT_LINE.GE.MAX_STRUCT_LINES) THEN
                     CALL DSA_DEF_ERROR(
     :                  'Too many structure elements defined',
     :                                 FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  STRUCT_LINE=STRUCT_LINE+1
                  ELEMENT_NAME(STRUCT_LINE)=WORD
                  ELEMENT_TYPE(STRUCT_LINE)=TYPE
                  IF (COND_DEF) THEN
                     ELEMENT_COND(STRUCT_LINE)=COND_VARIABLE
                  ELSE
                     ELEMENT_COND(STRUCT_LINE)=' '
                  END IF
               ELSE
C
C                 Line is unrecognisable
C
                  CALL DSA_DEF_ERROR('"'//WORD(:LWORD)//
     :                               '" is not a valid start to a line',
     :                                  FILE_LINE,FULL_NAME,LINE,STATUS)
                  GO TO 320      ! Abort line processing
               END IF
            END IF
         END IF
  320    CONTINUE      ! End of line processing
      END DO
C
C     Close down everything
C
  500 CONTINUE
      IGNORE=0
      IF (FOPEN) CALL DSA_FREE_LU(LU,IGNORE)
C
      END
C
      SUBROUTINE DSA_DEF_ERROR (ERROR,FILE_LINE,FULL_NAME,LINE,STATUS)
C
C     Utility routine for DSA_READ_STRUCT_DEF.  Outputs an error
C     description (ERROR), the name of the structure file in question
C     (FULL_NAME) the text (LINE) and line number (FILE_LINE) of the line
C     causing the error, and sets the status (STATUS) to an error value,
C     if it does not already indicate an error.  Should be regarded as an
C     internal routine of DSA_READ_STRUCT_DEF.
C
C     Parameters
C
      INTEGER FILE_LINE, STATUS
      CHARACTER*(*) ERROR, FULL_NAME, LINE
C
C     Functions used
C
      INTEGER ICH_LEN
      CHARACTER*8 ICH_CI
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER NUMBER*8                  ! Used to format line number
C
      IF (STATUS.EQ.0) THEN
         CALL DSA_WRUSER(
     :       'Error processing the structure definition file ')
         CALL DSA_WRUSER(FULL_NAME(:ICH_LEN(FULL_NAME)))
         CALL DSA_WRUSER(' at line ')
      ELSE
         CALL DSA_WRUSER('Further error at line ')
      END IF
      NUMBER=ICH_CI(FILE_LINE)
      CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
      CALL DSA_WRUSER(' -')
      CALL DSA_WRFLUSH
      CALL DSA_WRUSER(LINE(:ICH_LEN(LINE)))
      CALL DSA_WRFLUSH
      CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
      CALL DSA_WRUSER('.')
      CALL DSA_WRFLUSH
C
      IF (STATUS.EQ.0) STATUS=DSA__DEFERR
C
      END
