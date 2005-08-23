*  History:
*      1 Aug 2000 (ajc):
*        Replace hexadecimal constants
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_SEQUAL (STRING1, L1, STRING2, L2)

      IMPLICIT   NONE

*     Formal parameters:

      BYTE       STRING1(*)
      INTEGER    L1
      BYTE       STRING2(*)
      INTEGER    L2

*     Local variables;

      INTEGER    I
      INTEGER    L11, L22

      BYTE       BLANK
      BYTE       CHAR1
      BYTE       CHAR2

      DATA       BLANK /32/

*  Ok, go...

      GEN_SEQUAL = .FALSE.

*     Find true lengths of strings (drop blanks)

      L11 = L1
      DO WHILE (L11.GT.0 .and. STRING1(L11).EQ.BLANK)
        L11 = L11 - 1
      END DO

      L22 = L2
      DO WHILE (L22.GT.0 .and. STRING2(L22).EQ.BLANK)
        L22 = L22 - 1
      END DO

CD    PRINT *, 'Input strings have lengths: ', L11, L22

*     If strings are not same length, equality not satisfied

      IF (L11.NE.L22) RETURN

*     Test equality byte by byte (convert to upper case)

      DO I = 1, L11
        CHAR1 = STRING1(I)
        CHAR2 = STRING2(I)
        IF (CHAR1.GE.97 .and. CHAR1.LE.122) CHAR1 = CHAR1-32
        IF (CHAR2.GE.97 .and. CHAR2.LE.122) CHAR2 = CHAR2-32
        IF (CHAR1.NE.CHAR2) RETURN
      END DO

*     Survived that --- strings must be equivalent

      GEN_SEQUAL = .TRUE.

      RETURN
      END

*-----------------------------------------------------------------------
