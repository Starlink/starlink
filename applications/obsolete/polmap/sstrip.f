      SUBROUTINE SSTRIP(STRING)
C+
C
C Subroutine: 
C
C  S S T R I P
C
C
C Author: Ian Howath
C
C Parameters: 
C
C STRING (><)
C
C History: 
C  
C 
C
C  
C
C Subroutine to strip leading spaces from a character string
C
C
C
C
C
C-
      CHARACTER STRING*(*)
      INTEGER I
      IF ( STRING.NE.' ' ) THEN
        I = 1
10000   IF ( STRING(I:I).EQ.' ' ) THEN
          I = I+1
          GOTO 10000
        END IF
        STRING = STRING(I:)
      END IF
      END
