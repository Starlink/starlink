      INTEGER FUNCTION GRTRIM(S)
*+
*     - - - - - - - -
*       G R T R I M    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
* Find the length of a character string excluding trailing blanks.
* A blank string returns a value of 0.
*
*    Given:
*          S  c
*
*    Returned
*          GRTRIM    number of characters in S, excluding trailing
*                    blanks, in range 0...LEN(S). A blank string
*                    returns a value of 0.
*
*   D.L.Terrett  Starlink  Feb 1995 (after Tim Pearson)
*+
      IMPLICIT NONE

      CHARACTER*(*) S
      INTEGER  I

      IF (S.EQ.' ') THEN
          GRTRIM = 0
      ELSE
          DO 10 I=LEN(S),1,-1
              GRTRIM = I
              IF (S(I:I).NE.' ') GOTO 20
   10     CONTINUE
          GRTRIM = 0
   20     CONTINUE
      END IF
      END
