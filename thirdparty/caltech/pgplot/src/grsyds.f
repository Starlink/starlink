C*GRSYDS -- decode character string into list of symbol numbers
C+
      SUBROUTINE GRSYDS (SYMBOL, NSYMBS, TEXT, FONT)
      INTEGER SYMBOL(*), NSYMBS, FONT
      CHARACTER*(*) TEXT
C
C Given a character string, this routine returns a list of symbol
C numbers to be used to plot it. It is responsible for interpreting
C all escape sequences.  Negative `symbol numbers' are inserted in the
C list to represent pen movement. The following escape sequences are
C defined (the letter following the \ may be either upper or lower 
C case):
C
C \u       :      up one level (returns -1)
C \d       :      down one level (returns -2)
C \b       :      backspace (returns -3)
C \A       :      (upper case only) Angstrom symbol, roman font
C \x       :      multiplication sign
C \.       :      centered dot
C \\       :      \, returns the code for backslash
C \gx      :      greek letter corresponding to roman letter x
C \fn      :      switch to Normal font
C \fr      :      switch to Roman font
C \fi      :      switch to Italic font
C \fs      :      switch to Script font
C \mn or \mnn :   graph marker number n or nn (1 or 2 digits)
C \(nnn)   :      Hershey symbol number nnn (any number of digits)
C
C Arguments:
C  SYMBOL (output) : receives the list of symbol numers.
C  NSYMBS (output) : receives the actual number of symbols specified
C                    by the string; it is assumed that the dimension of
C                    SYMBOL is big enough (not less than LEN(TEXT)).
C  TEXT   (input)  : the text string to be decoded.
C  FONT   (input)  : the font number (1..4) to be used for decoding the
C                    string (this can be overridden by an escape
C                    sequence within the string).
C--
C  3-May-1983 - [TJP].
C 13-Jun-1984 - add \A [TJP].
C 15-Dec-1988 - standardize [TJP].
C 29-Nov-1990 - add \m escapes [TJP].
C 27-Nov-1991 - add \x escape [TJP].
C 27-Jul-1995 - extend for 256-character set [TJP]
C  7-Nov-1995 - add \. escape [TJP].
C-----------------------------------------------------------------------
      CHARACTER*8  FONTS
      CHARACTER*48 GREEK
      PARAMETER (FONTS = 'nrisNRIS')
      PARAMETER (GREEK = 'ABGDEZYHIKLMNCOPRSTUFXQW' //
     1                   'abgdezyhiklmncoprstufxqw' )
      INTEGER  CH, IG, J, LENTXT, IFONT, MARK
C
C Initialize parameters.
C
      IFONT = FONT
      LENTXT = LEN(TEXT)
      NSYMBS = 0
      J = 0
C
C Get next character; treat non-printing characters as spaces.
C
  100 J = J+1
      IF (J.GT.LENTXT) RETURN
      CH = ICHAR(TEXT(J:J))
      IF (CH.LT.0)   CH = 32
      IF (CH.GT.303) CH = 32
C
C Test for escape sequence (\)
C
      IF (CH.EQ.92) THEN
          IF ((LENTXT-J).GE.1) THEN
            IF (TEXT(J+1:J+1).EQ.CHAR(92)) THEN
                J = J+1
            ELSE IF (TEXT(J+1:J+1).EQ.'u' .OR.
     1                     TEXT(J+1:J+1).EQ.'U') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = -1
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'d' .OR.
     1                     TEXT(J+1:J+1).EQ.'D') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = -2
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'b' .OR.
     1                     TEXT(J+1:J+1).EQ.'B') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = -3
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'A') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = 2078
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'x') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = 2235
                IF (IFONT.EQ.1) SYMBOL(NSYMBS) = 727
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'.') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = 2236
                IF (IFONT.EQ.1) SYMBOL(NSYMBS) = 729
                J = J+1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'(') THEN
                NSYMBS = NSYMBS + 1
                SYMBOL(NSYMBS) = 0
                J = J+2
C               -- DO WHILE ('0'.LE.TEXT(J:J).AND.TEXT(J:J).LE.'9')
   90           IF ('0'.LE.TEXT(J:J).AND.TEXT(J:J).LE.'9') THEN
                  SYMBOL(NSYMBS) = SYMBOL(NSYMBS)*10 +
     1                      ICHAR(TEXT(J:J)) - ICHAR('0')
                   J = J+1
                GOTO 90
                END IF
C               -- end DO WHILE
                IF (TEXT(J:J).NE.')') J = J-1
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'m' .OR.
     1               TEXT(J+1:J+1).EQ.'M') THEN
                MARK = 0
                J = J+2
                IF ('0'.LE.TEXT(J:J).AND.TEXT(J:J).LE.'9') THEN
                    MARK = MARK*10 + ICHAR(TEXT(J:J)) - ICHAR('0')
                    J = J+1
                END IF
                IF ('0'.LE.TEXT(J:J).AND.TEXT(J:J).LE.'9') THEN
                    MARK = MARK*10 + ICHAR(TEXT(J:J)) - ICHAR('0')
                    J = J+1
                END IF
                J = J-1
                NSYMBS = NSYMBS + 1
                CALL GRSYMK(MARK, IFONT, SYMBOL(NSYMBS))
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'f' .OR.
     1               TEXT(J+1:J+1).EQ.'F') THEN
                IFONT = INDEX(FONTS, TEXT(J+2:J+2))
                IF (IFONT.GT.4) IFONT = IFONT-4
                IF (IFONT.EQ.0) IFONT = 1
                J = J+2
                GOTO 100
            ELSE IF (TEXT(J+1:J+1).EQ.'g' .OR.
     1               TEXT(J+1:J+1).EQ.'G') THEN
                IG = INDEX(GREEK, TEXT(J+2:J+2))
                NSYMBS = NSYMBS + 1
                CALL GRSYMK(255+IG, IFONT, SYMBOL(NSYMBS))
                J = J+2
                GOTO 100
            END IF
          END IF
      END IF
C
C Decode character.
C
      NSYMBS = NSYMBS + 1
      CALL GRSYMK(CH, IFONT, SYMBOL(NSYMBS))
      GOTO 100
      END
