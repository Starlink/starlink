      SUBROUTINE DSA_READ_STRUCT_DEF( FILENAME, STATUS )
*+
*  Name:
*     DSA_READ_STRUCT_DEF

*  Purpose:
*     Read one or more parameterised structure definitions from a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_READ_STRUCT_DEF( FILENAME, STATUS )

*  Description:
*     This routine reads a specified text file containing one or more
*     parameterised structure definitions. The values of the parameters
*     may be set later by calls to DSA_SET_STRUCT_VAR, and structures
*     corresponding to the definitions may then be created through calls
*     to routines such as DSA_CREATE_STRUCTURE.

*  Arguments:
*     FILENAME = CHARACTER * ( * ) (Given)
*        The name of the definition file to be used. The default
*        extension is '.def' and the usual Figaro directory search path
*        will be followed in looking for it.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine is extremely crude. In particular, the way "IF"
*     clauses are handled (the fact that they cannot be nested, for
*     example) is a limitation.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     28 Aug 1987. (ks):
*        Original version.
*     18 Mar 1987. (ks):
*        Comments completed.
*     15 Jan 1990. (ks):
*        Now supports variant structure types.
*     03 Mar 1991. (ks):
*        Now allows TAB characters in structure definitions. Uses
*        DSA_OPEN_TEXT_FILE instead of LIB$GET/FREE_LUN and FIG_OPFILE
*        to open the file.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     03 Sep 1992 (hme):
*        Changed default extension to lowercase.
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
      INCLUDE 'DSA_STRUCTURE'    ! Global variables for structure defs

*  Arguments Given:
      CHARACTER * ( * ) FILENAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL COND_DEF           ! Indicates in an "IF" clause
      LOGICAL EOF                ! Indicates end of definition file
      LOGICAL FOPEN              ! Indicates file opened
      LOGICAL STRUCT_DEF         ! Indicates in structure definition
      INTEGER FILE_LINE          ! Line number in definition file
      INTEGER FIRST              ! First non-blank char posn in line
      INTEGER FOR_STATUS         ! Fortran I/O system status
      INTEGER I                  ! General loop variable
      INTEGER IGNORE             ! Dummy status argument
      INTEGER LU                 ! Logical unit number
      INTEGER LWORD              ! Length of word
      INTEGER POSN               ! Scan pointer through record
      INTEGER STRUCT_LINE        ! Last used structure line entry
      INTEGER TABS(16)           ! TAB positions
      INTEGER VARIANT            ! Current variant, if any
      CHARACTER * ( 1 ) CHR      ! Used for any single character
      CHARACTER * ( 32 ) COND_VARIABLE ! Test condition for IF clause
      CHARACTER * ( 64 ) DEFAULT ! Default value for variable
      CHARACTER * ( 128 ) INPUT_LINE ! Line (possibly with TABS) read from file
      CHARACTER * ( 128 ) LINE   ! Line read from file - after detabbing
      CHARACTER * ( 64 ) NAME    ! Symbol name in file
      CHARACTER * ( 32 ) TYPE    ! Type of structure element
      CHARACTER * ( 64 ) UWORD   ! Upper case version of WORD
      CHARACTER * ( 64 ) WORD    ! Word delimited from input line

*  Internal References:
      INTEGER DSA3_WORD          ! Get a word from a string (ICH_WORD)

*.

*  Return immediately if bad status passed
      IF (STATUS.NE.0) RETURN

*  Some initial values
      FOPEN=.FALSE.
      SYMBOLS_DEFINED=0
      STRUCTS_DEFINED=0

*  Set standard 8 column TAB positions
      DO I=1,16
         TABS(I)=I*8+1
      END DO

*  Try to open the file
      CALL DSA_OPEN_TEXT_FILE(FILENAME,'.def','OLD',.FALSE.,LU,
     :                                               FULL_NAME,STATUS)
      IF (STATUS.NE.0) GO TO 500
      FOPEN=.TRUE.

*  Start to read through the definition file
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
            CALL ERR_MARK
               STATUS=SAI__ERROR
               CALL MSG_SETC('FDA_T010',FULL_NAME)
               CALL MSG_SETI('FDA_T023',FOR_STATUS)
               CALL ERR_REP('FDA_E074', 'DSA_READ_STRUCT_DEF: '//
     :            'I/O error while reading the structure '//
     :            'definition file ^FDA_T010. A Fortran error '//
     :            'occurred: IOSTAT = ^FDA_T023.',STATUS)
               CALL ERR_FLUSH(STATUS)
            CALL ERR_RLSE
            IF (STATUS.NE.0) STATUS=1
            GO TO 500
         ELSE

*        We have read a line OK from the file.  Ignore it if it is
*        blank or a comment, after first removing any tabs it may have.
            CALL DSA3_DETAB (INPUT_LINE,LINE,16,TABS)
            FILE_LINE=FILE_LINE+1
            CALL CHR_LDBLK (LINE)
            FIRST=1
            CHR=LINE(FIRST:FIRST)
            IF ((CHR.NE.' ').AND.(CHR.NE.'*')) THEN

*           It isn't a comment.  So, it ought to start with one of
*           the keywords (VARIANT,VARIABLE,STRUCTURE,END,IF or EQUATE)
*           or be a structure element definition, in which case it begins
*           with a '.'.  Note that the parsing performed by this
*           routine is rather 'brute force' in nature, and isn't
*           any sort of fancy parsing scheme - the allowed syntax is
*           sufficiently simple that we just about get away with this.
               POSN=DSA3_WORD(LINE,FIRST,' ',' ',WORD,LWORD,CHR)
               UWORD=WORD
               CALL CHR_UCASE (UWORD)
               IF (UWORD(:LWORD).EQ.'STRUCTURE') THEN

*              'STRUCTURE' - start of definition.
*              Line syntax is:   STRUCTURE name [type]
                  IF (STRUCT_DEF) THEN
                     CALL DSA3_DEFERR(
     :                   'Invalid nesting of structure definitions',
     :                              FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ',' ',NAME,LWORD,CHR)
                  ELSE
                     NAME=' '
                  END IF
                  IF (NAME.EQ.' ') THEN
                     CALL DSA3_DEFERR('No name given for structure',
     :                                FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ',' ',WORD,LWORD,CHR)
                  ELSE
                     WORD=' '
                  END IF
                  IF (STRUCTS_DEFINED.GE.MAX_STRUCTS) THEN
                     CALL DSA3_DEFERR('Too many structures defined',
     :                                FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  STRUCTS_DEFINED=STRUCTS_DEFINED+1
                  STRUCT_DEF=.TRUE.
                  CALL CHR_UCASE (NAME)
                  STRUCT_NAMES(STRUCTS_DEFINED)=NAME
                  STRUCT_START(STRUCTS_DEFINED)=STRUCT_LINE+1
                  STRUCT_TYPE(STRUCTS_DEFINED)=WORD
                  STRUCT_TYPE_FLAG(STRUCTS_DEFINED)=VARIANT
               ELSE IF (UWORD.EQ.'VARIABLE') THEN

*              'VARIABLE' - declaration
*              Line syntax is:  VARIABLE name [DEFAULT value]
                  IF (SYMBOLS_DEFINED.GE.MAX_SYMBOLS) THEN
                     CALL DSA3_DEFERR(
     :                  'Too many variables and/or equates defined',
     :                                  FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  SYMBOLS_DEFINED=SYMBOLS_DEFINED+1
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ',' ',NAME,LWORD,CHR)
                  ELSE
                     NAME=' '
                  END IF
                  IF (NAME.EQ.' ') THEN
                     CALL DSA3_DEFERR('No name given for variable',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  SYMBOL_NAMES(SYMBOLS_DEFINED)=NAME
                  SYMBOL_STATE(SYMBOLS_DEFINED)=UNDEFINED_VAR
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ',' ',WORD,LWORD,CHR)
                  ELSE
                     WORD=' '
                  END IF
                  DEFAULT=' '
                  IF (WORD.NE.' ') THEN
                     UWORD=WORD
                     CALL CHR_UCASE (UWORD)
                     IF (UWORD(:LWORD).NE.'DEFAULT') THEN
                        CALL DSA3_DEFERR(
     :                        'Unrecognised word encountered where '//
     :                                        '"DEFAULT" was expected',
     :                                  FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                     END IF
                     IF (POSN.GT.0) THEN
                        POSN=DSA3_WORD(LINE,POSN+1,' ','""',DEFAULT,
     :                                                       LWORD,CHR)
                     ELSE
                        DEFAULT=' '
                        CHR=' '
                     END IF
                     IF ((DEFAULT.EQ.' ').AND.(CHR.NE.'"')) THEN
                        CALL DSA3_DEFERR('Nothing follows "DEFAULT"',
     :                                  FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                     END IF
                     SYMBOL_VALUES(SYMBOLS_DEFINED)=DEFAULT
                     SYMBOL_STATE(SYMBOLS_DEFINED)=DEFINED_VAR
                  END IF
               ELSE IF (UWORD.EQ.'VARIANT') THEN

*              'VARIANT' - declaration
*              Line syntax is:  VARIANT type
                  IF (VARIANT.NE.ANY_TYPE) THEN
                     CALL DSA3_DEFERR(
     :                  'Attempt to nest VARIANT specifications.',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (STRUCT_DEF) THEN
                     CALL DSA3_DEFERR(
     :                   'Attempt to change variant within structure',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ',' ',NAME,LWORD,CHR)
                  ELSE
                     NAME=' '
                  END IF
                  IF (NAME.EQ.' ') THEN
                     CALL DSA3_DEFERR('No name given for variant',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  CALL CHR_UCASE (NAME)
                  IF (NAME.EQ.'NDF') THEN
                     VARIANT=NDF_TYPE
                  ELSE IF (NAME.EQ.'DST') THEN
                     VARIANT=DST_TYPE
                  ELSE
                     CALL DSA3_DEFERR('Variant must be DST or NDF',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
               ELSE IF (UWORD.EQ.'EQUATE') THEN

*              'EQUATE' - declaration
*              Line syntax is:  EQUATE name object
                  IF (SYMBOLS_DEFINED.GE.MAX_SYMBOLS) THEN
                     CALL DSA3_DEFERR(
     :                  'Too many equates and/or variables declared',
     :                                  FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  SYMBOLS_DEFINED=SYMBOLS_DEFINED+1
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ',' ',NAME,LWORD,CHR)
                  ELSE
                     NAME=' '
                  END IF
                  IF (NAME.EQ.' ') THEN
                     CALL DSA3_DEFERR('No name given for equate',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  SYMBOL_NAMES(SYMBOLS_DEFINED)=NAME
                  SYMBOL_TYPE_FLAG(SYMBOLS_DEFINED)=VARIANT
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ',' ',WORD,LWORD,CHR)
                  ELSE
                     WORD=' '
                  END IF
                  IF (WORD.EQ.' ') THEN
                     CALL DSA3_DEFERR('No value given for equate',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  SYMBOL_VALUES(SYMBOLS_DEFINED)=WORD
                  SYMBOL_STATE(SYMBOLS_DEFINED)=EQUATE
               ELSE IF (UWORD.EQ.'IF') THEN

*              'IF' - start of condition
*              Line syntax is:  IF variable [THEN]
                  IF (.NOT.STRUCT_DEF) THEN
                     CALL DSA3_DEFERR('"IF" clause appears '//
     :                                 'outside structure definition',
     :                                 FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (COND_DEF) THEN
                     CALL DSA3_DEFERR(
     :                   'Unfortunately, "IF" clauses cannot be nested',
     :                                 FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ','() ',
     :                  NAME,LWORD,CHR)
                  ELSE
                     NAME=' '
                  END IF
                  IF (NAME.EQ.' ') THEN
                     CALL DSA3_DEFERR('No condition specified',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  COND_VARIABLE=NAME
                  COND_DEF=.TRUE.
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ',' ',WORD,LWORD,CHR)
                     CALL CHR_UCASE (WORD)
                  ELSE
                     WORD=' '
                  END IF
                  IF ((WORD.NE.' ').AND.(WORD.NE.'THEN')) THEN
                     CALL DSA3_DEFERR(
     :                   'Unrecognised word where "THEN" was expected',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
               ELSE IF (UWORD(:LWORD).EQ.'END') THEN

*              'END' - end of definition.
*              Line syntax is:   END STRUCTURE or END IF or END VARIANT
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ',' ',NAME,LWORD,CHR)
                     CALL CHR_UCASE (NAME)
                  ELSE
                     NAME=' '
                  END IF
                  IF ((NAME.NE.' ').AND.(NAME.NE.'STRUCTURE').AND.
     :                (NAME.NE.'VARIANT').AND.(NAME.NE.'IF')) THEN
                     CALL DSA3_DEFERR('Invalid "END"',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (NAME.EQ.'VARIANT') THEN
                     IF (STRUCT_DEF) THEN
                        CALL DSA3_DEFERR('Cannot change variant '//
     :                               'within a structure definition',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                        GO TO 320      ! Abort line processing
                     END IF
                     VARIANT=ANY_TYPE
                  ELSE
                     IF (.NOT.STRUCT_DEF) THEN
                        CALL DSA3_DEFERR(
     :                   'Invalid nesting of structure definitions',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                        GO TO 320      ! Abort line processing
                     END IF
                     IF (COND_DEF) THEN
                        IF (NAME.EQ.'STRUCTURE') THEN
                           CALL DSA3_DEFERR(
     :                          'Structure ended with "IF" unclosed',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                           GO TO 320      ! Abort line processing
                        END IF
                        COND_DEF=.FALSE.
                        COND_VARIABLE=' '
                     ELSE
                        IF (NAME.EQ.'IF') THEN
                           CALL DSA3_DEFERR(
     :                                '"END IF" with no "IF" to end',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                           GO TO 320      ! Abort line processing
                        END IF
                        STRUCT_DEF=.FALSE.
                        STRUCT_END(STRUCTS_DEFINED)=STRUCT_LINE
                     END IF
                  END IF
               ELSE IF (UWORD(1:1).EQ.'.') THEN

*              Structure element definition - starts with '.'
*              Line syntax is: .element type
                  IF (.NOT.STRUCT_DEF) THEN
                     CALL DSA3_DEFERR('Structure element appears '//
     :                                 'outside structure definition',
     :                                 FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (POSN.GT.0) THEN
                     POSN=DSA3_WORD(LINE,POSN+1,' ',' ',TYPE,LWORD,CHR)
                  ELSE
                     TYPE=' '
                  END IF
                  IF (TYPE.EQ.' ') THEN
                     CALL DSA3_DEFERR(
     :                    'No type specified for structure element',
     :                               FILE_LINE,FULL_NAME,LINE,STATUS)
                     GO TO 320      ! Abort line processing
                  END IF
                  IF (STRUCT_LINE.GE.MAX_STRUCT_LINES) THEN
                     CALL DSA3_DEFERR(
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

*              Line is unrecognisable
                  CALL DSA3_DEFERR('"'//WORD(:LWORD)//
     :                               '" is not a valid start to a line',
     :                                  FILE_LINE,FULL_NAME,LINE,STATUS)
                  GO TO 320      ! Abort line processing
               END IF
            END IF
         END IF
  320    CONTINUE      ! End of line processing
      END DO

*  Close down everything
  500 CONTINUE
      IGNORE=0
      IF (FOPEN) CALL DSA_FREE_LU(LU,IGNORE)
      END



      SUBROUTINE DSA3_DEFERR (ERROR,FILE_LINE,FULL_NAME,LINE,STATUS)

*  Utility routine for DSA_READ_STRUCT_DEF.  Outputs an error
*  description (ERROR), the name of the structure file in question
*  (FULL_NAME) the text (LINE) and line number (FILE_LINE) of the line
*  causing the error, and sets the status (STATUS) to an error value,
*  if it does not already indicate an error.  Should be regarded as an
*  internal routine of DSA_READ_STRUCT_DEF.

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

      CHARACTER * ( * ) ERROR
      INTEGER FILE_LINE
      CHARACTER * ( * ) FULL_NAME
      CHARACTER * ( * ) LINE

      INTEGER STATUS

      INTEGER LSTAT

      INTEGER CHR_LEN

      CHARACTER*255 STRING

      IF (STATUS.EQ.0) THEN
         STRING =  'Error processing the structure definition file '//
     :      FULL_NAME(:CHR_LEN(FULL_NAME))//' at line '
         CALL MSG_SETC('FDA_T024', STRING )
      ELSE
         CALL MSG_SETC('FDA_T024','Further error at line ')
      END IF
      CALL MSG_SETI('FDA_T025',FILE_LINE)
      STRING = ' -'//LINE(:CHR_LEN(LINE))//' '//
     :  ERROR(:CHR_LEN(ERROR))//'.'
      CALL MSG_SETC('FDA_T026', STRING )
      CALL ERR_MARK
         LSTAT=SAI__ERROR
         CALL ERR_REP('FDA_E075', 'DSA_READ_STRUCT_DEF: '//
     :            '^FDA_T024^FDA_T025^FDA_T026',LSTAT)
         CALL ERR_FLUSH(LSTAT)
      CALL ERR_RLSE
      IF (STATUS.EQ.0) STATUS=1

      END



      SUBROUTINE DSA3_DETAB (IN,OUT,NTAB,TABS)

*  Copy of GEN_DETAB.

      IMPLICIT NONE

      INTEGER NTAB, TABS(NTAB)
      CHARACTER*(*) IN, OUT

      INTEGER I              ! Loop index through chars in IN.
      INTEGER ITAB           ! Index of current tab position
      INTEGER LENGTH         ! Chars already in OUT
      LOGICAL TABINC         ! Indicates TABPOS is still too low
      LOGICAL TABOK          ! Indicates TABPOS is still valid
      INTEGER TABPOS         ! Position of next available tab stop

      CHARACTER TAB
*      PARAMETER (TAB=CHAR(9))

      TAB=CHAR(9)
      ITAB=0
      OUT=' '
      LENGTH=0
      TABPOS=0
      DO I=1,LEN(IN)
         LENGTH=LENGTH+1
         IF (LENGTH.GT.LEN(OUT)) GO TO 500   ! Break from I loop
         IF (IN(I:I).NE.TAB) THEN
            OUT(LENGTH:LENGTH)=IN(I:I)
         ELSE
            TABOK=TABPOS.GT.LENGTH
            IF (.NOT.TABOK) THEN
               IF (ITAB.LT.NTAB) THEN
                  TABINC=.TRUE.
                  DO WHILE (TABINC)
                     ITAB=ITAB+1
                     IF (ITAB.GT.NTAB) THEN
                        TABINC=.FALSE.
                        TABPOS=LENGTH
                     ELSE
                        TABPOS=TABS(ITAB)
                        TABINC=TABPOS.LE.LENGTH
                     END IF
                  END DO
               END IF
            END IF
            LENGTH=TABPOS-1
         END IF
      END DO

  500 CONTINUE
      END



      INTEGER FUNCTION DSA3_WORD(STRING,IST,DELIMS,QUOTES,WORD,
     :                                                  LWORD,CHAR)

*  Copy of ICH_WORD.

      IMPLICIT NONE

      INTEGER IST, LWORD
      CHARACTER*(*) STRING, DELIMS, QUOTES, WORD, CHAR

      LOGICAL BLANK
      INTEGER ACTION, CHTYPE, I, IPTR, LSTR, POSN, STATE
      CHARACTER CHR*1, QUOTER

      INTEGER LB, QW, IW, EQW, TB, EX
      INTEGER DEL, QUO, BLA, OTH, EN
      INTEGER POS, POS0, WRD, POSM1, ERR
      PARAMETER (LB=1, QW=2, IW=3, EQW=4, TB=5, EX=6)
      PARAMETER (DEL=1, QUO=2, BLA=3, OTH=4, EN=5)
      PARAMETER (POS=1, POS0=2, WRD=3, POSM1=4, ERR=5)

      INTEGER STATES(EN,EX-1), ACTS(EN,EX-1)
      DATA STATES/
     :      EX,      QW,       LB,      IW,       EX,
     :      QW,      EQW,      QW,      QW,       EX,
     :      EX,      EX,       TB,      IW,       EX,
     :      EX,      QW,       TB,      EX,       EX,
     :      EX,      EX,       TB,      EX,       EX/
      DATA ACTS/
     :      POS,     0,        0,       WRD,      POS0,
     :      WRD,     0,        WRD,     WRD,      ERR,
     :      POS,     POSM1,    0,       WRD,      POS0,
     :      POS,     WRD,      0,       POSM1,    POS0,
     :      POS,     POSM1,    0,       POSM1,    POS0/

      LSTR=LEN(STRING)
      WORD=' '
      CHAR=' '
      LWORD=0
      IF ((IST.LT.0).OR.(IST.GT.LSTR)) THEN
         POSN=0
         GO TO 500
      END IF

      BLANK=INDEX(DELIMS,' ').NE.0

      IPTR=IST
      STATE=LB
      DO WHILE (STATE.NE.EX)

         IF (IPTR.GT.LSTR) THEN
            CHTYPE=EN
         ELSE
            CHR=STRING(IPTR:IPTR)

            CHTYPE=OTH

            IF (BLANK.AND.(CHR.EQ.' ')) THEN
               CHTYPE=BLA
            ELSE

               DO I=1,LEN(DELIMS)
                  IF (CHR.EQ.DELIMS(I:I)) THEN
                     CHTYPE=DEL
                     GO TO 340        ! Break loop
                  END IF
               END DO
  340          CONTINUE
               IF (CHTYPE.NE.DEL) THEN

                  IF ((STATE.EQ.IW).OR.(STATE.EQ.LB)) THEN
                     IF (QUOTES.NE.' ') THEN
                        DO I=1,LEN(QUOTES),2
                           IF (CHR.EQ.QUOTES(I:I)) THEN
                              CHAR=CHR
                              CHTYPE=QUO
                              IF (I.LT.LEN(QUOTES)) THEN
                                 QUOTER=QUOTES(I+1:I+1)
                              ELSE
                                 QUOTER=CHR
                              END IF
                              GO TO 360       ! Break loop
                           END IF
                        END DO
  360                   CONTINUE
                     END IF
                  ELSE IF ((STATE.EQ.QW).OR.(STATE.EQ.EQW)) THEN
                     IF (CHR.EQ.QUOTER) CHTYPE=QUO
                  END IF
               END IF
            END IF
         END IF

         ACTION=ACTS(CHTYPE,STATE)
         STATE=STATES(CHTYPE,STATE)

         IF (ACTION.GT.0) THEN
            IF (ACTION.EQ.POS) THEN
               POSN=IPTR
            ELSE IF (ACTION.EQ.POS0) THEN
               POSN=0
            ELSE IF (ACTION.EQ.POSM1) THEN
               POSN=IPTR-1
            ELSE IF (ACTION.EQ.ERR) THEN
               POSN=-2
            ELSE IF (ACTION.EQ.WRD) THEN
               LWORD=LWORD+1
               IF (LWORD.GT.LEN(WORD)) THEN
                  LWORD=LEN(WORD)
               ELSE
                  WORD(LWORD:LWORD)=CHR
               END IF
            END IF
         END IF
         IPTR=IPTR+1
      END DO

  500 CONTINUE
      IF (POSN.GE.LEN(STRING)) POSN=-1
      DSA3_WORD=POSN

      END
