      SUBROUTINE hlp_LINOUT (OUTSUB, LOUT, INDENT, BUFFER, J)
*+
*  - - - - - - -
*   L I N O U T
*  - - - - - - -
*
*  Output a line of help information, indented to the right.
*
*  Given:
*     OUTSUB    x      user-supplied output routine
*     LOUT      i      maximum record length accepted by OUTSUB
*     INDENT    i      number of spaces to indent by
*     BUFFER    c*(*)  line before insertion of leading spaces
*
*  Returned:
*     BUFFER    c*(*)  line after insertion of leading spaces
*     J         i      status from OUTSUB call: 1=OK
*
*  Notes:
*
*     1)  The calling sequence for OUTSUB is J=OUTSUB(string), where
*         string is a character string of length LOUT or less.  The
*         status J is +1 for success.
*
*     2)  If INDENT is less than (or equal to) zero, BUFFER is output
*         as it is.
*
*  Called:  hlp_LENGTH
*
*  P.T.Wallace   Starlink   13 June 1995
*-

      IMPLICIT NONE

      INTEGER OUTSUB
      EXTERNAL OUTSUB
      INTEGER LOUT,INDENT
      CHARACTER*(*) BUFFER
      INTEGER J

      INTEGER hlp_LENGTH

      INTEGER I,ITO,IFROM
      CHARACTER C



*  Number of spaces to insert at the front of the string.
      I=MAX(INDENT,0)

*  Insert something into every character starting from the right.
      DO ITO=LEN(BUFFER),1,-1

*     Get the next character from the string, or a space.
         IFROM=ITO-I
         IF (IFROM.GE.1) THEN
            C=BUFFER(IFROM:IFROM)
         ELSE
            C=' '
         END IF

*     Store it.
         BUFFER(ITO:ITO)=C

*     Next character.
      END DO

*  Output the line (without trailing spaces).
      J=OUTSUB(BUFFER(:MIN(LOUT,hlp_LENGTH(BUFFER))))

      END
