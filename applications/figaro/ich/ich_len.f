C+
      INTEGER FUNCTION ICH_LEN(STRING)
C
C     I C H _ L E N
C
C     Returns the 'logical length' of a character string.
C
C     Parameter -      (">" input, "<" output)
C
C     (>) STRING    (Character) The string whose length is required.
C
C     Returns -
C
C     (<) ICH_LEN   (Integer) The number - starting from 1 - of the
C                   last non-blank character in STRING.  Under some
C                   circumstances this is the logical length of the
C                   string.  If the string is completely blank, ICH_LEN
C                   returns 0.
C
C                                         KS / UCL  8th June 1982
C+
      CHARACTER*(*) STRING
C
      ICH_LEN=0
      DO I=1,LEN(STRING)
         IF (STRING(I:I).NE.' ') THEN
            ICH_LEN=I
         END IF
      END DO
C
      END
