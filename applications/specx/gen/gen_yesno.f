*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*      1 Aug 2000 (ajc):
*        Re-write illegal concatenation
*-----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_YESNO(PROMPT,DEFAULT,TEST,IERR)

C  Routine to get a logical value from the terminal, using a yes/no type
C  of question. The option (Y/N) is always appended to the prompt, and
C  the default value depends on the input value of DEFAULT. TEST is set
C  to TRUE if the first character of the answer (CH) is Y or y, FALSE
C  otherwise.

      INTEGER*4 LP
      CHARACTER PROMPT*(*),CHDEF*1,CH*1
      LOGICAL   DEFAULT,TEST

      INTEGER*4 GEN_ILEN
      CHARACTER*80 PSTRING

* Ok, go..

      GEN_YESNO = .TRUE.
      CHDEF     = 'N'
      IF (DEFAULT) CHDEF = 'Y'

      LP = GEN_ILEN (PROMPT)
      PSTRING = PROMPT(:LP)
      PSTRING(LP+1:) = ' (Y/N)'
      CALL GEN_GETSTR2 (1, PSTRING, CHDEF, 'A1', CH, IERR)
      IF (IERR.LT.0)  RETURN

      CALL UUCASE (CH)
      IF (CH(:1).EQ.'Y' .OR. CH(:1).EQ.'T') THEN
        TEST = .TRUE.
      ELSE IF (CH(:1).EQ.'N' .OR. CH(:1).EQ.'F') THEN
        TEST = .FALSE.
      ELSE
        TEST = .FALSE.
        IERR = -1
      END IF

      RETURN
      END
