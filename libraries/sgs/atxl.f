      SUBROUTINE sgs_ATXL (STRING)
*+
*   - - - - -
*    A T X L
*   - - - - -
*
*   Append left justified string onto text buffer.
*
*   Given:
*      STRING   c*(*)   character string
*
*   Externals:
*      sgs_ATEXT
*
*  P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING

      INTEGER I


*  Find length of left justified portion
      DO 10 I = LEN(STRING),1,-1
         IF (STRING(I:I).NE.' ') GO TO 20
   10 CONTINUE
      I = 0
   20 CONTINUE

*  Append
      IF (I.GE.1) CALL sgs_ATEXT(STRING(:I))

      END
