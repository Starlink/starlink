      INTEGER FUNCTION hlp_LENGTH (STRING)
*+
*  - - - - - - -
*   L E N G T H
*  - - - - - - -
*
*  Length of a string excluding any trailing spaces.
*
*  Given:
*     STRING       c*(*)    the string
*
*  Returned:
*     hlp_LENGTH   i        length excluding trailing spaces
*
*  The minimum length returned is 1.
*
*  P.T.Wallace   Starlink   24 February 1991
*-

      IMPLICIT NONE

      CHARACTER STRING*(*)

      INTEGER L,I



      L=LEN(STRING)
      I=1
      DO WHILE (I.LE.L.AND.STRING(I:).NE.' ')
         I=I+1
      END DO
      hlp_LENGTH=MAX(1,I-1)

      END
