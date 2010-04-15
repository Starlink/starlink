*  History:
*     30 Nov 1993 (hme):
*        Declare PRIORITY as BYTE, since what gets passed down is an
*        element of a BYTE array, and usually not aligned to be treated
*        as INTEGER (never mind getting the correct information into the
*        correct byte).
*     13 Oct 2000 (ajc):
*        Report invalid operator
*-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION GEN_PARSEOP (STRING, ST, IST, IFIN,
     &                                PRIORITY, RBRACKET, NEXT, IERR)

*  Routine to find next operator in the expression, to encode which one
*  it is, and to assign a execution-priority.
*  Routine updated 22/12/90 by RP to include relational and logical operators.

      IMPLICIT  NONE

*     Formal parameters

      CHARACTER STRING*(*)     ! string to be parsed
      INTEGER*4 ST             ! starting position in string
      INTEGER*4 IST, IFIN      ! begin and end positions of output token
      BYTE      PRIORITY       ! Arithmetic priority of returned operator
      LOGICAL*4 RBRACKET       ! Right bracket encountered
      INTEGER*4 NEXT           ! start position of next token
      INTEGER*4 IERR           ! error return

*     Error returns
*
*        IERR = 2        ! Empty string
*        IERR = 3        ! Non operator item

*     Functions

      INTEGER*4 GEN_ILEN

*     Local variables

      INTEGER*4 ILS
      CHARACTER CHAR*1

*  Make sure it "falls through" if error return already set

      GEN_PARSEOP = .TRUE.
      IF (IERR.NE.0) RETURN

*  Ok, go..

      ILS  = GEN_ILEN (STRING)
      NEXT = ST

      IST  = ST
      IFIN = ST

      RBRACKET    = .FALSE.

*  Check on string length

      IF (IST.GT.ILS) THEN
        IERR = 2
        RETURN
      END IF

*  Suppress leading blanks.

      DO WHILE (STRING(NEXT:NEXT).EQ.' ' .AND. NEXT.LE.ILS)
        NEXT = NEXT + 1
      END DO

*  This can't take us past the end of the string? Well, actually it can,
*  since we didn't measure the length of the string ourselves but inherited
*  it as a parameter. So just check again..

      IF (NEXT.GT.ILS) THEN
        IERR = 2
        RETURN
      END IF

      IST = NEXT

*  Check that next item is an allowed operator.

      CHAR = STRING(IST:IST)

      IF (CHAR .EQ. '^') THEN
        PRIORITY = 6
        NEXT     = NEXT + 1

      ELSE IF (CHAR .EQ. '*') THEN
        PRIORITY = 5
        NEXT     = NEXT + 1

      ELSE IF (CHAR .EQ. '/') THEN
        PRIORITY = 5
        NEXT     = NEXT + 1

      ELSE IF (CHAR .EQ. '+') THEN
        PRIORITY = 4
        NEXT     = NEXT + 1

      ELSE IF (CHAR .EQ. '-') THEN
        PRIORITY = 4
        NEXT     = NEXT + 1

      ELSE IF (CHAR.EQ.'=' .OR. CHAR.EQ.'>' .OR. CHAR.EQ.'<') THEN
        PRIORITY = 3
        NEXT     = NEXT + 1
        CHAR     = STRING(NEXT:NEXT)
        IF (CHAR.EQ.'=' .OR. CHAR.EQ.'>' .OR. CHAR.EQ.'<') NEXT=NEXT+1

      ELSE IF (CHAR .EQ. '~') THEN
        PRIORITY = 2
        NEXT     = NEXT + 1

      ELSE IF (CHAR .EQ. '&') THEN
        PRIORITY = 1
        NEXT     = NEXT + 1

      ELSE IF (CHAR .EQ. '!') THEN
        PRIORITY = 1
        NEXT     = NEXT + 1

      ELSE IF (CHAR .EQ. ')') THEN
        PRIORITY = 0
        NEXT     = NEXT + 1
        RBRACKET = .TRUE.

      ELSE
        PRIORITY = 0
      END IF

      IFIN = NEXT - 1
      IF (IFIN.LT.IST) THEN
        PRINT *, '-- gen_parseop --'
        PRINT *, '   Invalid operator "',string(ist:next),'"'
        IERR = 3
        GO TO 99
      END IF

      RETURN

*  Error return

   99 CONTINUE
      GEN_PARSEOP = .FALSE.
      RETURN

      END
