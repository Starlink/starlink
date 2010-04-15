      SUBROUTINE GRSKPB (S, I)
*+
*   - - - - - - - -
*     G R S K P B
*   - - - - - - - -
*
*   skip blanks in character string
*
*   Increment I so that it points to the next non-blank
*   character in string S.  'Blank' characters are space and tab (ASCII
*   character value 9).
*
* Arguments:
*  S      (input)  : character string to be parsed.
*  I      (in/out) : on input, I is the index of the first character
*                    in S to be examined; on output, either it points
*                    to the next non-blank character, or it is equal
*                    to LEN(S)+1 (if all the rest of the string is
*                    blank).
*
*   D.L.Terrett  Starlink  Sep 1993
*-
      IMPLICIT NONE

      CHARACTER*(*) S
      INTEGER I

   10 IF (I.GT.LEN(S)) RETURN
      IF (S(I:I).NE.' ' .AND. S(I:I).NE.CHAR(9)) RETURN
      I = I+1
      GOTO 10
      END
