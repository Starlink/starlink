C+
      INTEGER FUNCTION ICH_VERIF(STRING,IST,CHARS)
C
C     I C H _ V E R I F
C
C     Returns the position of the first character in a string
C     that is not one of a specified set of characters.
C
C     Parameters -   (">" input, "<" output)
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
C     (<) ICH_VERIF  (Integer) The number of the first character
C                    in the string - from IST on - which is not
C                    one of the characters in CHARS.  If there
C                    is no such character, ICH_VERIF returns 0.
C
C     Example:
C
C     ICH_IVERIF('1234567890ABCDE',1,'0123456789')
C
C     will return the value 11 (the position of the 'A')
C
C     ICH_IVERIF('     ABCDEFG  ',6,'ABCDEF')
C
C     will return the value 12 (the position of the 'G'
C     after the 'A' which is the 6th character)
C
C                                    KS / UCL  15th June 1982
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
      ICH_VERIF=0
      DO I=NST,LEN(STRING)
         IF (INDEX(CHARS,STRING(I:I)).EQ.0) THEN
            ICH_VERIF=I
            GO TO 340
         END IF
      END DO
  340 CONTINUE
C
      END
