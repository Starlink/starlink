
* JM removed declare_* data declarations

       SUBROUTINE DEFTYP(STRING,TYPE,OK)
*
*   Supplies default file type ('TYPE') to string
*   representing file name ('STRING')
*
       IMPLICIT NONE

       CHARACTER*(*) STRING, TYPE
       LOGICAL OK

       INTEGER LDOT, LBRACE
       INTEGER I, LLNGTH

*   Convert to upper case and strip leading blanks

       CALL DTOUPP(STRING)
       CALL SSTRIP(STRING)

*   STRING is empty, TYPE represents full file specification

       LLNGTH = INDEX(STRING,' ') - 1
       IF (LLNGTH.EQ.0) THEN
          IF (LEN(STRING).GE.LEN(TYPE)) THEN
             STRING = TYPE
          ELSE
             OK = .FALSE.
          ENDIF
          GOTO 300
       ENDIF

*   Supply '.TYP' if no value exists

       DO 100 I = LLNGTH, 1, -1
          LDOT = I
          IF (STRING(I:I).EQ.'.') GOTO 200
  100  CONTINUE
       LDOT = 0
  200  CONTINUE
       LBRACE = INDEX(STRING,']')
       IF (LDOT.LE.LBRACE) THEN
          IF (LLNGTH+4.LE.LEN(STRING)) THEN
             STRING(LLNGTH+1:LLNGTH+4) = '.'//TYPE(1:3)
             LLNGTH = LLNGTH + 4
          ELSE
             OK = .FALSE.
          ENDIF
       ENDIF

  300  CONTINUE

       END
