      SUBROUTINE sgs_ATEXT (STRING)
*+
*   - - - - - -
*    A T E X T
*   - - - - - -
*
*   Append a field onto the text buffer.
*
*   If the resulting string is longer than the text buffer, characters
*   will be lost from the end.
*
*   Given:
*      STRING   c*(*)   String to be appended
*
*   Read from COMMON:
*      NTEXT    i       Length of current string
*      CTEXT    c()     Text string buffer
*
*   Constants (from SGSCOM):
*      LTEXT    i       Size of text string buffer
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING

      INCLUDE 'sgscom'


      INTEGER LS,NCHARS


*  Accept append request only if text string has been begun
      IF (NTEXT.GE.0) THEN

*     Append up to end of buffer
         LS=LEN(STRING)
         NCHARS=MIN(LS,LTEXT-NTEXT)
         IF (NCHARS.GT.0) CTEXT(NTEXT+1:NTEXT+NCHARS)=STRING(:NCHARS)

*     Update count as if whole string had been appended
         NTEXT=NTEXT+LS
      END IF

      END
