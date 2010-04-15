*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION GEN_PARSENAME (STRING, ST, IST, IFIN, NUMERIC,
     &                                  STRCONST, UMINUS, LBRACKET,
     &                                  FUNCTION, NEXT, IERR)


      IMPLICIT  NONE

*     Formal parameters

      CHARACTER STRING*(*)     ! string to be parsed
      INTEGER*4 ST             ! starting position in string
      INTEGER*4 IST, IFIN      ! begin and end positions of output token
      LOGICAL*4 NUMERIC        ! Purely numeric input
      LOGICAL*4 STRCONST       ! Input was a string constant
      LOGICAL*4 UMINUS         ! Unary minus required
      LOGICAL*4 LBRACKET       ! Operand of unary operator is bracketed
      LOGICAL*4 FUNCTION       ! Operand followed by argument list
      INTEGER*4 NEXT           ! start position of next token
      INTEGER*4 IERR           ! error return

*     Error returns
*
*        IERR = 2        ! Empty string
*        IERR = 3        ! Non symbol item

*     Functions

      INTEGER*4 GEN_ILEN

*     Local variables

      LOGICAL*4 CONTINUE
      INTEGER*4 ILS
      CHARACTER CHAR*1

*  Ok, go...

      GEN_PARSENAME = .TRUE.

*     Arrange for the routine to "fall through" if error return already set

      IF (IERR.NE.0) RETURN

      ILS  = GEN_ILEN (STRING)
      NEXT = ST

      IST  = ST
      IFIN = ST

      UMINUS        = .FALSE.
      FUNCTION      = .FALSE.
      LBRACKET      = .FALSE.

CD    PRINT *, '-- gen_parsename --'

*  Suppress leading blanks.

    1 CONTINUE
      DO WHILE (STRING(NEXT:NEXT).EQ.' ' .AND. NEXT.LE.ILS)
        NEXT = NEXT + 1
      END DO

CD    PRINT *, '    blanks suppressed, IST, NEXT, IFIN = ', IST,NEXT,IFIN

*  If this now takes us past the end of the string then string is empty

      IF (NEXT.GT.ILS) THEN
        IERR = 2
        GO TO 99
      END IF

*  Check that the value is not to be negated.. (and ignore a + sign)

      IF (STRING(NEXT:NEXT).EQ.'-') THEN
        NEXT   = NEXT + 1
        UMINUS = .NOT.UMINUS
        GO TO 1
      ELSE IF (STRING(NEXT:NEXT).EQ.'+') THEN
        NEXT   = NEXT + 1
        GO TO 1
      ELSE IF (STRING(NEXT:NEXT).EQ.'~') THEN
        NEXT   = NEXT + 1
        UMINUS = .NOT. UMINUS
        GO TO 1
      END IF

      IST = NEXT


CD    PRINT *, '    unary ops done, IST, NEXT, IFIN = ', IST,NEXT,IFIN

*  Parse on a character by character basis. Rules are to keep going
*  until a character is found which is:
*      (i)   Non alphanumeric (both cases OK).
*  or  (ii)  Not a left bracket.
*  or  (iii) Not enclosed between a left bracket and its corresponding r bracket
*  or  (iv)  Not a stop (.)
*  Thus strings like "record.type(4)(4:4)" should get through, but
*  "record*type(4)" should return just "record" leaving "*" to be picked up
*  as an operator. On the other hand, "(record*type(4))" is OK.

      CHAR = STRING(NEXT:NEXT)
      CALL UUCASE (CHAR)

      STRCONST = (CHAR.EQ.'''')
      NUMERIC  = (CHAR.GE.'0' .AND. CHAR.LE.'9') .OR. (CHAR.EQ.'.')
      LBRACKET = (CHAR.EQ.'(')

CD    PRINT *, '    1st char parsed, IST, NEXT, IFIN = ', IST,NEXT,IFIN
CD    PRINT *, '    STRCONST, NUMERIC, LBRACKET ',
CD   &             strconst, numeric, lbracket

      IF (LBRACKET) THEN
        ST = NEXT
CD      PRINT *, '    left bracket found - returning...'
        RETURN
      END IF

      CONTINUE = .TRUE.

      IF (NUMERIC) THEN
        NEXT = NEXT + 1
        CALL GEN_PARSEEXP (STRING, ILS, NEXT)

      ELSE IF (STRCONST) THEN
        NEXT = NEXT + 1
        DO WHILE (NEXT.LE.ILS .AND. CONTINUE)
          IF (STRING(NEXT:NEXT).NE.'''') THEN
            NEXT = NEXT + 1
          ELSE IF (STRING(NEXT:NEXT+1).EQ.'''''' ) THEN
            NEXT = NEXT + 2
          ELSE
            NEXT = NEXT + 1
            CONTINUE = .FALSE.
          END IF
CD        Print *, '    next = ', next
        END DO

      ELSE
        DO WHILE (NEXT.LE.ILS .AND. CONTINUE)
          CHAR = STRING(NEXT:NEXT)
          CALL UUCASE (CHAR)

*         If character is one of the allowed set then continue

          IF (     (CHAR.GE.'0' .AND. CHAR.LE.'9')    ! A numeral
     &        .OR. (CHAR.GE.'A' .AND. CHAR.LE.'Z')    ! A letter
     &        .OR. (CHAR.EQ.'_')                      ! An underscore
     &        .OR. (CHAR.EQ.'.')) THEN                ! A period (.)
            NEXT = NEXT + 1

*         Else NEXT must now point to one past the end of the string:

          ELSE
            CONTINUE = .FALSE.
          END IF
        END DO

      END IF

      IFIN = NEXT - 1

CD    PRINT *, '    string parsed, IST, NEXT, IFIN = ', IST,NEXT,IFIN

*     Check that we got something

      IF (IFIN.LT.IST) THEN
        IERR = 3
        GO TO 99
      END IF

*     Advance to next non-blank character

      DO WHILE (STRING(NEXT:NEXT).EQ.' ' .AND. NEXT.LE.ILS)
        NEXT = NEXT + 1
      END DO

CD    PRINT *, '    advance next ch, IST, NEXT, IFIN = ', IST,NEXT,IFIN

*     If not end of string, check if next character is left bracket
*     -- if it is then set the function flag (but might be an array too)

      IF (NEXT.GT.ILS) THEN
CD      PRINT *,'    end of string - returning...'
        RETURN
      ELSE IF (STRING(NEXT:NEXT).EQ.'(') THEN
        FUNCTION = .TRUE.
CD      PRINT *,'    function or array argument - returning...'
      END IF

      RETURN

*  Error return

   99 CONTINUE
      GEN_PARSENAME = .FALSE.
      RETURN

      END
