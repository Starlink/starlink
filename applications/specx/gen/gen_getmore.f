*  History:
*     01 Feb 1995 (rpt):
*        Changed code to use SPROMPT (see also GEN_PUTPMT, GEN_GETLINE)
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Only check IERR if GEN_PUTPMT is called
*        Unused GEN_ILEN
*      22 Aug 2005 (timj):
*        Use CHAR(9) for TAB init
*-----------------------------------------------------------------------

      SUBROUTINE GEN_GETMORE (PROMPT, STRING, JDEF)

C   Routine to interrogate a file for some more input

      CHARACTER PROMPT*(*), STRING*(*), SPROMPT*132, TAB

      INCLUDE  'CLI_STACK.INC'

      TAB = CHAR(9)

      JDEF = 0
      IF (ISP.eq.0) THEN
         CALL GEN_PUTPMT (PROMPT, IERR, SPROMPT)
         IF (IERR.ne.0)   THEN
           CALL GEN_ERMSG (IERR)
           JDEF = -1
           RETURN
         END IF
      END IF

      CALL GEN_GETLINE (STRING, SPROMPT, JDEF)
      IF (JDEF.lt.0)   THEN
        CALL GEN_ERMSG (-JDEF)
        RETURN
      ELSE IF (JDEF.EQ.2) THEN
*       PRINT *, ' -- gen_getmore -- JDEF', JDEF
        RETURN
      END IF

C   Search for tabs and replace with blanks

      DO I = 1, LEN(STRING)
        IF (STRING(I:I).eq.TAB)   STRING(I:I) = ' '
      END DO

      RETURN
      END
