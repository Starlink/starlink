      LOGICAL FUNCTION NUMBER(CHAR)
*+
*   Tests Whether the Character is a Number
*   Where this Includes Decimal Points(.) & Signs(+-)
*
*   Gets
*   ----
*      CHAR   -  CHARACTER*1 Variable Holding the Character
*-
      CHARACTER*1 CHAR

      N = ICHAR(CHAR)
      NUMBER = N.GE.ICHAR('0').AND.N.LE.ICHAR('9').OR.CHAR.
     : EQ.'.'.OR.CHAR.EQ.'+'.OR.CHAR.EQ.'-'
      END

