*  History:
*     16 Nov 1993 (hme):
*        Replace a backslash in a string constant with CHAR(92).
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Eliminate call to CHR_UCASE
*      1 Aug 2000 (ajc):
*        Missing commas in FORMAT
*        Change TYPE * to PRINT *
*        Unused in SCL_PREPARSE: J
*-----------------------------------------------------------------------

      SUBROUTINE SCL_PREPARSE (STRING, COMMAND, ERROR)

*  Routine to parse the command line to convert the forms:
*           x = value             --->   = x value
*           symbol := string      --->   := symbol string
*           name:                 --->   LABEL name
*           $vmscommand           --->   $ vmscmd
*           @command_file         --->   @ command_file
*  Command coming in is assumed upper case

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER STRING*(*)
      CHARACTER COMMAND*(*)
      INTEGER*4 ERROR

*     Functions

      INTEGER*4 GEN_ILEN

*     Local variables

      INTEGER*4 I, ILS
      CHARACTER BL*1
      DATA      BL /' '/

      INTEGER*4 IST
      INTEGER*4 IEND
      INTEGER*4 NEXT
      INTEGER*4 IERR

*  Ok? go..

      ILS   = GEN_ILEN (STRING)
      ERROR = 0

*  Remove leading blanks

      I = 1
      DO WHILE (I.LT.ILS .AND. STRING(I:I).EQ.' ')
        I = I + 1
      END DO

*  Debug output

CD    PRINT *,'input string  --> ', string(i:60)

*  Parse:

*     Look for a VMS command, (with or without space to allow selection of '$')
      IF (STRING(I:I).EQ.'$') THEN
        COMMAND = '$'
        STRING  = STRING(I+1:ILS)

*     Similarly for indirect command file (with or without space)
      ELSE IF (STRING(I:I).EQ.'@') THEN
        COMMAND = '@'
        STRING  = STRING(I+1:ILS)

*     Command symbol assignment
      ELSE IF (STRING(I:I+1).EQ.':=') THEN
        COMMAND = ':='
        STRING  = STRING(I+2:ILS)

*     Variable assignment
      ELSE IF (STRING(I:I).EQ.'=') THEN
        COMMAND = '='
        STRING  = STRING(I+1:ILS)

*     Else parse a bit more carefully
      ELSE

        CALL SCL_PARSE (STRING, I, IST, IEND, NEXT, IERR)

CD      Print *,'Parsed command: ', string(ist:iend)

        IF (STRING(NEXT:NEXT+1) .EQ. ':=') THEN
          COMMAND = ':='
          STRING  = STRING(IST:IEND) // BL // STRING(NEXT+2:)

        ELSE IF (STRING(NEXT:NEXT).EQ.'=') THEN
          COMMAND = '='
          STRING  = STRING(IST:IEND) // BL // STRING(NEXT+1:)

        ELSE IF (STRING(NEXT:NEXT).EQ.':') THEN
          COMMAND = 'LABEL'
          STRING  = STRING(IST:IEND) // ' '

        ELSE
          COMMAND = STRING(IST:IEND)
          IF (STRING(NEXT:NEXT).EQ.CHAR(92).OR.
     :        STRING(NEXT:NEXT).EQ.';') THEN
            NEXT = NEXT + 1
            DO WHILE (STRING(NEXT:NEXT).EQ.' ' .AND. NEXT.LE.ILS)
              NEXT = NEXT + 1
            END DO
          END IF
          STRING  = STRING(NEXT:)
        END IF
      END IF

*  Debug output

CD    PRINT *,'output command --> ', command
CD    PRINT *,'output string  --> ', string(:60)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SCL_PARSE (STRING, ST, IST, IFIN, NEXT, IERR)


      IMPLICIT  NONE

*     Formal parameters

      CHARACTER STRING*(*)     ! string to be parsed
      INTEGER*4 ST             ! starting position in string
      INTEGER*4 IST, IFIN      ! begin and end positions of output token
      INTEGER*4 NEXT           ! start position of next token
      INTEGER*4 IERR           ! error return

*     Error returns
*
*        IERR = 1        ! No matching brackets
*        IERR = 2        ! Empty string
*        IERR = 3        ! Non symbol item

*     Functions

      INTEGER*4 GEN_ILEN

*     Local variables

      LOGICAL*4 CONTINUE
      INTEGER*4 ILS
      INTEGER*4 NEST
      CHARACTER CHAR*1

      IERR = 0

*  Ok, go..

      ILS  = GEN_ILEN (STRING)
      NEST = 0
      NEXT = ST

      IST  = ST
      IFIN = ST

CD    Print *,'-- scl_parse --'
CD    Print *,'   input string: ', string(st:ils)

*  Suppress leading blanks.

    1 CONTINUE
      DO WHILE (STRING(NEXT:NEXT).EQ.' ' .AND. NEXT.LE.ILS)
        NEXT = NEXT + 1
      END DO

*  If this now takes us past the end of the string then string is empty

      IF (NEXT.GT.ILS) THEN
        IERR = 2
        GO TO 99
      END IF

      IST = NEXT

*  Parse on a character by character basis. Rules are to keep going
*  until a character is found which is:
*      (i)   Non alphanumeric (both cases OK).
*  or  (ii)  Not a left bracket.
*  or  (iii) Not enclosed between a left bracket and its corresponding r bracket
*  or  (iv)  Not a stop (.), hyphen (-) or underscore (_)

      CONTINUE = .TRUE.
      DO WHILE (NEXT.LE.ILS .AND. CONTINUE)
        CHAR = STRING(NEXT:NEXT)

*       If character is one of the allowed set then continue

        IF (     (CHAR.GE.'0' .AND. CHAR.LE.'9')    ! A numeral
     &      .OR. (CHAR.GE.'A' .AND. CHAR.LE.'Z')    ! A capital letter
     &      .OR. (CHAR.GE.'a' .AND. CHAR.LE.'z')    ! A small letter
     &      .OR. (CHAR.EQ.'-')                      ! A hyphen
     &      .OR. (CHAR.EQ.'_')                      ! A underscore
     &      .OR. (CHAR.EQ.'.')) THEN                ! A period (.)
          NEXT = NEXT + 1

*       Else NEXT must now point to one past the end of the string:

        ELSE
          CONTINUE = .FALSE.
        END IF
      END DO

*     If not end of string, check if next character is left bracket
*     -- if it is then advance to end of brackets.

      IFIN = NEXT - 1

      IF (NEXT.GT.ILS) THEN
        RETURN
      ELSE IF (STRING(NEXT:NEXT).EQ.'(') THEN
        NEST = 1
        NEXT = NEXT + 1
        DO WHILE (NEST.NE.0 .AND. NEXT.LE.ILS)
          IF (STRING(NEXT:NEXT).EQ.'(') NEST = NEST + 1
          IF (STRING(NEXT:NEXT).EQ.')') NEST = NEST - 1
          NEXT = NEXT + 1
        END DO
      END IF

*     Check that brackets closed

      IF (NEST.NE.0) THEN
        IERR = 1
        GO TO 99
      END IF

      IFIN = NEXT - 1

*     Check that we got something

      IF (IFIN.LT.IST) THEN
        IERR = 3
        GO TO 99
      END IF

*     Advance to next non-blank character

      DO WHILE (STRING(NEXT:NEXT).EQ.' ' .AND. NEXT.LE.ILS)
        NEXT = NEXT + 1
      END DO

      RETURN

*  Error return

   99 CONTINUE
      RETURN

      END

*-----------------------------------------------------------------------
