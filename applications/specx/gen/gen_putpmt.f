*  History:
*     16 Nov 1993 (hme):
*        Disuse OUTERR_HANDLER error handler.
*     30 Nov 1993 (hme):
*        Instead of the format '$',A<ILS>,' ' use A,' ',$. I.e., use
*        the $ descriptor instead of the $ carriage control character.
*        But also, try to use a default field width for A.
*     01 Feb 1995 (rpt):
*        Changed code to decode PROMPT into a new prompt string SPROMPT
*        for use by GET_LINE, rather than writing PROMPT to the temrinal
*     31 July 2000 (ajc):
*        Re-write illegal concatenation
*        Correct LUNOUT to LUN_OUT (add IMPLICIT NONE and define various
*        other variables)
*-----------------------------------------------------------------------

      SUBROUTINE GEN_PUTPMT(PROMPT,IERR, SPROMPT)

C   Routine to put a prompt message to the terminal and leave cursor
C   positioned immediately after the end.
C   If the prompt string PROMPT begins and ends with the 'alternate'
C   string delimiters " then the string is just used as the format
C   for a write to the terminal. Within the string all '' strings are
C   interpreted as ' characters (by the compiler).
C   Otherwise the string is interpreted as a single line message and
C   sent to the terminal so that a trailing blank is added and the
C   cursor is left positioned at the end of the line.
C   The first facility may be useful if multi-line prompts are required, as
C   when explaining the available options.
C
C   Note that this does NOT affect the use of constant character expressions
C   in straightforward calls with single line prompts.

      IMPLICIT NONE

      CHARACTER PROMPT*(*), SPROMPT*(*), TPROMPT*255
      CHARACTER BLANK*1
      CHARACTER QUOT*1
      CHARACTER*512 PSTRING

      INCLUDE  'LOGICAL_UNITS.INC'

      INTEGER*4 GEN_ILEN
      INTEGER*4 GEN_IENDCH
      INTEGER*4 IERR
      INTEGER*4 ILS, JLS
      INTEGER*4 I

      DATA QUOT / '"' /
      DATA BLANK / ' ' /

*  Ok..go
      IERR = 0
      IF (LUN_OUT_SET.EQ.0) LUN_OUT = 6

*     Need to establish length and then remove hollerith characters if there
      ILS = GEN_ILEN (PROMPT)
      IF (PROMPT(1:1).EQ.'''' .AND. PROMPT(ILS:ILS).EQ.'''') THEN
        IF (GEN_IENDCH (PROMPT).EQ.ILS) THEN
          CALL GEN_HDNORM (PROMPT, PROMPT, ILS, IERR)
        END IF
      END IF

*   Decide whether prompt is complete format string by checking if first
*   and last characters are alternate string delimiters (").

      IF (IERR.NE.0 .OR. ILS.EQ.0) THEN
        RETURN
      ELSE IF( PROMPT(1:1).EQ.QUOT .AND. PROMPT(ILS:ILS).EQ.QUOT) THEN
        IF (PROMPT(ILS:ILS).NE.QUOT) THEN
           ILS = ILS + 1
        ENDIF
        I = ILS-1
        JLS = ILS-1
        DO WHILE (I .GT. 1 .AND. JLS .EQ. (ILS-1) )
          IF (PROMPT(I:I).EQ.'/') JLS = I
          I = I - 1
        ENDDO
        PSTRING(1:4) = '(1X,'
        PSTRING(5:) = PROMPT(2:JLS)
        PSTRING(JLS+4:) = ',$)'
        WRITE (LUN_OUT, PSTRING(1:JLS+6), ERR=100, IOSTAT=IERR)
        PSTRING(1:1) = '('
        PSTRING(2:) = PROMPT(JLS+1:ILS-1)
        PSTRING(ILS-JLS+1:) = ')'
        WRITE (TPROMPT, PSTRING(1:ILS-JLS+2), ERR=100, IOSTAT=IERR)
        ILS = GEN_ILEN (TPROMPT)
        JLS = 1
        DO WHILE (TPROMPT(JLS:JLS) .EQ. ' ' .AND. JLS .LT. ILS)
          JLS = JLS + 1
        ENDDO
        DO WHILE (TPROMPT(ILS:ILS) .EQ. ' ' .AND. ILS .GT. JLS)
          ILS = ILS - 1
        ENDDO
        WRITE (SPROMPT, 1000, ERR=101, IOSTAT=IERR) TPROMPT(JLS:ILS)
        RETURN
      ELSE
        WRITE (SPROMPT, 1000, ERR=100, IOSTAT=IERR) PROMPT(1:ILS)
        RETURN
      END IF

C  Error transfer

  100 WRITE (LUN_OUT,*) 'Offending string is:'
      WRITE (LUN_OUT,*) PROMPT
      RETURN

  101 WRITE (LUN_OUT,*) 'Offending multiple-line string is:'
      WRITE (LUN_OUT,*) TPROMPT
      RETURN

 1000 FORMAT (1X,A,' ',$)

      END
