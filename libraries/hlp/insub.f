      INTEGER FUNCTION hlp_INSUB (STRING, PROMPT, L)
*+
*  - - - - - -
*   I N S U B
*  - - - - - -
*
*  A minimalist example of the user-supplied INSUB routine, which is
*  called by the hlp_HELP routine to obtain an interactive response.
*
*
*  Returned:
*     STRING      c*(*)    response string
*
*  Given:
*     PROMPT      c*(*)    prompt string
*
*  Returned:
*     L           i        length of string excluding trailing blanks
*     hlp_INSUB   i        status: always 1=OK
*
*  Called:  hlp_LENGTH
*
*  I/O units:
*     LUCMD   input    command
*     LUMES   output   prompt
*
*  Note:  The calling sequence for this routine is the same as that for
*         the VAX/VMS routine LIB$GET_INPUT.
*
*  P.T.Wallace   Starlink   28 July 1992
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING,PROMPT
      INTEGER L

*  I/O unit numbers
      INTEGER LUCMD,LUMES
      PARAMETER (LUCMD=5,LUMES=6)     !!! Machine dependent !!!

      INTEGER hlp_LENGTH


      WRITE (LUMES,'(A,$)') PROMPT     !!! Sun SPARC etc
      READ (LUCMD,'(A)') STRING
      L=hlp_LENGTH(STRING)
      hlp_INSUB=1

      END
