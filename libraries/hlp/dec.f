      SUBROUTINE hlp_DEC (STRING, IPTR, NUM)
*+
*  - - - -
*   D E C
*  - - - -
*
*  Decode a decimal integer from a character string.
*
*  Given:
*     STRING    c*(*)   string containing decimal digits
*     IPTR      i       string index for start of decode
*
*  Returned:
*     IPTR      i       advanced one character past last decimal
*     NUM       i       number, or -1 if none found
*
*  Notes:
*
*  1)  Leading zeroes or spaces are both acceptable.
*
*  2)  Decoding ends with the first non-decimal.
*
*  3)  No sign is permitted.
*
*  4)  If the decode is ended by end-of-string, IPTR is returned with
*      a value one greater than the length of the string.
*
*  5)  No decode is attempted if IPTR is initially outside the
*      string.
*
*  P.T.Wallace   Starlink   28 July 1992
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER IPTR,NUM

*  Decode state:  0 = accepting leading spaces
*                 1 = accepting digits
*                 2 = number terminated
      INTEGER ISTATE

      INTEGER IZERO,N,I,IDIGIT
      CHARACTER C



*  Collating-sequence value for digit zero.
      IZERO=ICHAR('0')

*  State = waiting for first digit.
      ISTATE=0

*  Default result.
      N=-1

*  Initialize pointer.
      I=IPTR-1

*  Loop until end-of-string or end-of-number.
      DO WHILE (I.GE.0.AND.I.LT.LEN(STRING).AND.ISTATE.LT.2)

*     Next character.
         I=I+1
         C=STRING(I:I)

*     Space?
         IF (C.EQ.' ') THEN

*        Yes: if we have seen numbers this is the end.
            IF (ISTATE.GT.0) ISTATE=2

*     Digit?
         ELSE IF (C.GE.'0'.AND.C.LE.'9') THEN

*        Yes: convert to integer.
            IDIGIT=ICHAR(C)-IZERO

*        First digit to be seen?
            IF (ISTATE.EQ.0) THEN

*           Yes: start the number and set the state.
               N=IDIGIT
               ISTATE=1
            ELSE

*           Not the first: include it.
               N=IDIGIT+N*10
            END IF
         ELSE

*        Unrecognized character: terminate.
            ISTATE=2
         END IF

*     Next character.
      END DO

*  Results.
      IPTR=I
      NUM=N

      END
