*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused GEN_IENDCH
C-----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_GETSTR (PROMPT, SDEF, FDEF, STRING, JDEF)

      IMPLICIT  NONE

C   Routine to put a prompt message to the terminal and receive a
C   single string value returned in STRING. JDEF as for GETCH
C   SDEF is the default value for STRING, FDEF is a format qualifier
C   e.g. A3, describing how the default is to be presented by the
C   prompt. A null string for FDEF will suppress the presentation
C   of the default value.

*     Formal parameters

      CHARACTER PROMPT*(*)
      CHARACTER FDEF*(*)
      CHARACTER STRING*(*)
      CHARACTER SDEF*(*)
      INTEGER*4 JDEF

*     Local variables

      INTEGER   LV           ! Level at which to fetch next "item"
      INTEGER   ILS          ! String length
      INTEGER   LS           ! Returned string length
      INTEGER   IERR
      INTEGER   SYM_INDEX
      INTEGER   LENGTH
      INTEGER   ADDRESS
      LOGICAL   READONLY
      LOGICAL   EVALUATE
      CHARACTER TYPE*4
      CHARACTER STR*256

*     Functions

      LOGICAL   STRING_TEST
      LOGICAL   GEN_GETSTR2
      LOGICAL   GEN_ALPHANUM
      INTEGER   GEN_ILEN

*     Go...

      LV = 1
      GEN_GETSTR = GEN_GETSTR2 (LV, PROMPT, SDEF, FDEF, STR, JDEF)

C     Test if string constant (enclosed in hollerith delimiters) and
C     if so remove them; otherwise test to see if it translates as a
C     a string expression.

      ILS      = GEN_ILEN (STR)
      STRING   = ' '
      EVALUATE = .FALSE.

CD    PRINT *, '-- gen_getstr --'
CD    PRINT *, '   passed string is ', STR(:ILS)

      IF (JDEF.EQ.1) THEN
CD      PRINT *, '   JDEF = 1; accepting default'
        STRING = STR(:ILS) // ' '
      ELSE IF (JDEF.EQ.2) THEN
CD      PRINT *, '   JDEF = 2; return with ^Z set'
        CONTINUE
      ELSE IF (STRING_TEST(STR, LS, IERR)) THEN
CD      PRINT *, '   Good hollerith string; accept it'
        STRING = STR(:LS) // ' '
      ELSE IF (GEN_ALPHANUM (STR(:ILS))) THEN
CD      PRINT *, '   String is alphanumeric; test for string-symbol'
        CALL GEN_INQSYMB (STR(:ILS), SYM_INDEX, TYPE, LENGTH,
     &                    ADDRESS, READONLY, IERR)
        IF (SYM_INDEX.EQ.0 .OR. TYPE(1:1).NE.'C') THEN
CD        PRINT *, '   String not a string-symbol name; use as is'
          STRING = STR(:ILS) // ' '
        ELSE
CD        PRINT *, '   String is a string-symbol name; evaluate'
          EVALUATE = .TRUE.
        END IF
      ELSE
CD      PRINT *, '   String is an expression? - evaluate it'
        EVALUATE = .TRUE.
      END IF

      IF (EVALUATE) THEN
CD      PRINT *, '   Treat string as expression: evaluate'
        WRITE             (TYPE, '(''C'',I3.3)') LEN (STRING)
        CALL GEN_EVAL_AE  (STR(:ILS), TYPE, %REF(STRING), IERR)
      END IF

CD    PRINT *, '   nominal string length = ', LEN(STRING)
CD    PRINT *, '   returned string = ', STRING
CD    PRINT *, '   returned length = ', GEN_ILEN(STRING)

      RETURN
      END
