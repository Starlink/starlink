C+
      CHARACTER FUNCTION ICH_LOWER(CHR)
C
C     I C H _ L O W E R
C
C     Returns the lower-case character corresponding to an
C     upper-case character.
C
C     Parameter -     (">" input, "<" output)
C
C     (>) CHR         (Character) The character in question.
C                     If CHR is a string, only the first character
C                     is considered.
C
C     Returns -
C
C     (<) ICH_LOWER   (Character) If CHR is an upper-case character,
C                     the corresponding lower-case character is returned.
C                     Otherwise, the character as passed is returned.
C
C     Note that this version is for ASCII characters.
C
C                                           KS / UCL  17th June 1982
C+
      CHARACTER*(*) CHR
C
      IF ((CHR(1:1).GE.'A').AND.(CHR(1:1).LE.'Z')) THEN
         ICH_LOWER=CHAR(ICHAR(CHR(1:1))+32)
      ELSE
         ICH_LOWER=CHR(1:1)
      END IF
C
      END
