      SUBROUTINE sgs_ATXB (STRING, NSPACE)
*+
*   - - - - -
*    A T X B
*   - - - - -
*
*   Append a field onto the text buffer leaving a specified number
*   of spaces before the non-blank region of the field.
*
*   Given:
*      STRING   c*(*)   String to be appended
*      NSPACE   i       Number of spaces to leave
*
*   Externals:
*      sgs_ATEXT
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      INTEGER NSPACE
      CHARACTER*(*) STRING

      INTEGER N,IS,LS


*   String length
      LS=LEN(STRING)

*   Append the required number of spaces
      IF (NSPACE.GE.1) THEN
         DO 10 N=1,NSPACE
            CALL sgs_ATEXT(' ')
   10    CONTINUE
      END IF

*   Skip leading spaces in field
      DO 20 IS = 1,LS
         IF (STRING(IS:IS).NE.' ') GO TO 30
   20 CONTINUE
   30 CONTINUE

*  Append rest of field
      CALL sgs_ATEXT(STRING(IS:))

      END
