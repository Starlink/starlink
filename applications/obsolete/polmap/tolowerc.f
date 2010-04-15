      SUBROUTINE TOLOWERC(STRING)
C+
C
C Subroutine:
C
C    T O L O W E R C
C
C
C Author: Ian Howarth
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
C
C
C Sets a string to lower case
C
C
C
C-

      CHARACTER STRING*(*)
      INTEGER I
      INTEGER LENSTR, NEWCH
      CHARACTER CHA*1

      LENSTR = LEN(STRING)
      DO 1 I = 1,LENSTR
        CHA = STRING(I:I)
        IF ( LGE(CHA,'A') .AND. LLE(CHA,'Z') ) THEN
          NEWCH = ICHAR(CHA)+ICHAR('a')-ICHAR('A')
          STRING(I:I) = CHAR(NEWCH)
        END IF
    1 CONTINUE

      END
