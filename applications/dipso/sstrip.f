      SUBROUTINE SSTRIP(STRING)
C
C Subroutine to strip leading spaces from a character string
C
      CHARACTER STRING*(*)
      INTEGER I
      IF ( STRING.NE.' ' ) THEN
        I=1
10000   IF ( STRING(I:I).EQ.' ' ) THEN
          I=I+1
          GOTO 10000
        END IF
        STRING=STRING(I:)
      END IF
      END
