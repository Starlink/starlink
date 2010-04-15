C+
      INTEGER FUNCTION ICH_KEY(STRING,IST,DELIMS,KEYS,EXACT,NEXT)
C
C     I C H _ K E Y
C
C     Looks for a keyword in a character string.
C
C     Parameters -      (">" input, "<" output)
C
C     (>) STRING      (Character) The string to be examined.
C
C     (>) IST         (Integer) The first character in the string
C                     to be looked at.  ICH_KEY will start at this
C                     character, then skip any leading blanks and
C                     check the characters that follow against the
C                     keyword list.
C
C     (>) DELIMS      (Character) The set of characters that are
C                     allowed to terminate the keyword in STRING.
C
C     (>) KEYS        (Character) The set of keywords, separated
C                     and terminated by the same delimiting
C                     character.
C
C     (>) EXACT       (Character) If this string starts with 'E'
C                     or 'e', the match between the keyword as
C                     found in STRING and in KEYS must be exact.
C                     Otherwise, only the number of characters
C                     found in STRING will be tested, and the first
C                     keyword in KEYS that matches them will be
C                     accepted.
C
C     (<) NEXT        (Integer) The number of the character
C                     following the delimiting character in STRING.
C                     If the delimiting character was the last
C                     in the string, or if the keyword was
C                     terminated by the end of STRING, NEXT is
C                     returned as zero.
C
C     Returns -
C
C     (<) ICH_KEY     (Integer) The number of the matching keyword.
C                     If no keyword matches, ICH_KEY returns zero.
C
C     Example -
C
C     ICH_KEY('  OPT3',1,' ','OPT1;OPT2;OPT3;OPT4','Exact',NEXT)
C
C     will return 3, with NEXT set to 0
C
C     ICH_KEY('IGNORED;OPT2;',9,' ;','OPT1:OPT2:OPT3:','Abbr.',NEXT)
C
C     will return 2, with NEXT set to 13
C
C                                         KS / UCL  19th Sept 1982
C     Modified:
C
C     10th June 1985.  KS / AAO.  Terminating char in KEYS is now last
C                      non-blank char, not last char in string.
C+
      CHARACTER*(*) STRING,DELIMS,KEYS,EXACT
      INTEGER IST,NEXT
C
      INTEGER IPTR,LENGTH,LAST,LENKS,KPTR,KNEXT,LENKEY,NST
      INTEGER INVOKE
      CHARACTER CHR
      LOGICAL ABBR
C
      IF ((IST.LT.1).OR.(IST.GT.LEN(STRING))) THEN
         NST=1
      ELSE
         NST=IST
      END IF
C
C     Initial values
C
      CHR=EXACT(1:1)
      INVOKE=ICH_FOLD(CHR)
      ABBR=CHR.NE.'E'
      KEY=0
      LENKS=ICH_LEN(KEYS)
      CHR=KEYS(LENKS:LENKS)
      NEXT=0
C
C     Look for first non-blank character (if any)
C
      IPTR=ICH_VERIF(STRING,NST,' ')
      IF (IPTR.NE.0) THEN
C
C        There is something in the string, so get its length
C
         NEXT=ICH_DELIM(STRING,IPTR,DELIMS)
         LENGTH=NEXT-IPTR
C
C        Allow for termination by end of string
C
         IF (NEXT.EQ.0) THEN
            LENGTH=LEN(STRING)-IPTR+1
         END IF
         NEXT=NEXT+1
         IF (NEXT.GT.LEN(STRING)) THEN
            NEXT=0
         END IF
C
         LAST=IPTR+LENGTH-1
C
C        Now work through the keywords, one by one
C
         KPTR=1
         KNEXT=0
         DO WHILE (KNEXT.LT.LENKS)
            KEY=KEY+1
            KNEXT=ICH_DELIM(KEYS,KPTR,CHR)
            LENKEY=KNEXT-KPTR
            IF ((LENKEY.EQ.LENGTH).OR.
     :            (ABBR.AND.(LENKEY.GE.LENGTH))) THEN
               IF (STRING(IPTR:LAST).EQ.KEYS(KPTR:KPTR+LENGTH-1))
     :                                                       THEN
                  GO TO 340
               END IF
            END IF
         KPTR=KNEXT+1
         END DO
C
C        If loop wasn't broken, no match can have been found
C
         KEY=0
C
  340    CONTINUE
C
      END IF
      ICH_KEY=KEY
C
      END
