*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION STRING_TEST (STRING, LS, ISTAT)

*  Function that returns TRUE if passed string is bounded by hollerith
*  delimiters. If it is it strips them out and returns the "native" string

      IMPLICIT  NONE

*     Formal parameters

      CHARACTER STRING*(*)
      INTEGER*4 LS
      INTEGER*4 ISTAT

*     Local variables:

      INTEGER*4 IST, IEND

*     Functions:

      INTEGER*4 GEN_ILEN

*  OK, go..

      STRING_TEST = .FALSE.
      IEND = GEN_ILEN (STRING)

*     Skip over leading blanks

      IST = 1
      DO WHILE (STRING(IST:IST).EQ.' ' .AND. IST.LE.IEND)
        IST = IST + 1
      END DO

*     Test that first non-blank character is "'"

      IF (STRING(IST:IST).NE.'''') RETURN

*     Error if no matching "'" at far end of string

      STRING_TEST = .TRUE.
      IF (STRING(IEND:IEND).NE.'''') THEN
        PRINT *, '-- string_test --'
        PRINT *, '  Badly formed string ', STRING(IST:)
        ISTAT = 1
      END IF

*     Remove hollerith delimiters and convert ''s to 's

      CALL GEN_HDNORM (STRING(IST:IEND), STRING(1:IEND), LS, ISTAT)

      RETURN
      END
