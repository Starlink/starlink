C+
      INTEGER FUNCTION ICH_DELIM(STRING,IST,CHARS)
C
C     I C H _ D E L I M
C
C     Returns the position of the first character in a string
C     that matches one of a specified set of characters.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) STRING     (Character) The string to be examined.
C
C     (>) IST        (Integer) The number of the character at
C                    which the search should start.  Characters
C                    are numbered from 1 up.  If IST is 0 or
C                    negative the search starts at the start of
C                    the string - as though IST were 1.
C
C     (>) CHARS      (Character) The specified set of characters.
C
C     Returns -
C
C     (<) ICH_DELIM  (Integer) The number of the first character
C                    in the string - from IST on - which is also
C                    one of the characters in CHARS.  If there
C                    is no such character, ICH_DELIM returns 0.
C
C     Note that the name comes from the use of this routine
C     to search for delimiters in a string.
C
C     Example:
C
C     ICH_DELIM('This is a string with a semi-colon ;  ',1,CHARS)
C
C     will return 5 if CHARS=' ',  36 if CHARS=';',
C     and 5 if CHARS='; '
C
C                                      KS / UCL  15th June 1982
C+
      CHARACTER*(*) STRING,CHARS
      INTEGER IST
C
      INTEGER NST,I
C
      IF ((IST.LT.1).OR.(IST.GT.LEN(STRING))) THEN
         NST=1
      ELSE
         NST=IST
      END IF
C
      ICH_DELIM=0
      DO I=NST,LEN(STRING)
         IF (INDEX(CHARS,STRING(I:I)).NE.0) THEN
            ICH_DELIM=I
            GO TO 340
         END IF
      END DO
  340 CONTINUE
C
      END
