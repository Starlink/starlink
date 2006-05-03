      SUBROUTINE sgs_ATEXT (STRING)
*+
*  Name:
*     ATEXT

*  Purpose:
*     Append a field onto the text buffer.

*  Language:
*     Starlink Fortran 77

*  Description:
*     If the resulting string is longer than the text buffer, characters
*     will be lost from the end.

*  Arguments:
*     STRING = CHAR (Given)
*         String to be appended

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Read From Common:
*     NTEXT    i       Length of current string
*     CTEXT    c()     Text string buffer
*
*  Constants (from SGSCOM):
*     LTEXT    i       Size of text string buffer

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
