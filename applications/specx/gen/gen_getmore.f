*  History:
*     01 Feb 1995 (rpt):
*        Changed code to use SPROMPT (see also GEN_PUTPMT, GEN_GETLINE)
*-----------------------------------------------------------------------

      SUBROUTINE GEN_GETMORE (PROMPT, STRING, JDEF)

C   Routine to interrogate a file for some more input

      INTEGER   GEN_ILEN
      CHARACTER PROMPT*(*), STRING*(*), SPROMPT*132, TAB
      DATA      TAB/09/

      INCLUDE  'CLI_STACK.INC'

      JDEF = 0
      IF (ISP.eq.0)   CALL GEN_PUTPMT (PROMPT, IERR, SPROMPT)
      IF (IERR.ne.0)   THEN
        CALL GEN_ERMSG (IERR)
        JDEF = -1
        RETURN
      END IF
 
      CALL GEN_GETLINE (STRING, SPROMPT, JDEF)
      IF (JDEF.lt.0)   THEN
        CALL GEN_ERMSG (-JDEF)
        RETURN
      ELSE IF (JDEF.EQ.2) THEN
*       TYPE *, ' -- gen_getmore -- JDEF', JDEF
        RETURN
      END IF

C   Search for tabs and replace with blanks

      DO I = 1, LEN(STRING)
        IF (STRING(I:I).eq.TAB)   STRING(I:I) = ' '
      END DO

      RETURN      
      END
