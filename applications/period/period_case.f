
      SUBROUTINE PERIOD_CASE(STRING, LCASE)

C=======================================================================
C Routine to convert the case of a STRING from UPPER->LOWER (for
C LCASE = .FALSE.) or from LOWER->UPPER (for LCASE = .TRUE.).
C
C Written by Vik Dhillon and Paul Devine @Sussex 2-Mar-1992.
C=======================================================================

      IMPLICIT NONE

      INTEGER ASCVAL, ILEN, I
      LOGICAL LCASE
      CHARACTER*(*) STRING

      ILEN = LEN(STRING)
      IF ( LCASE ) THEN
         DO 50 I = 1, ILEN
            IF ( (ICHAR(STRING(I:I)).GE.97) .AND.
     :           (ICHAR(STRING(I:I)).LE.122) ) THEN
               ASCVAL = ICHAR(STRING(I:I)) - 32
               STRING(I:I) = CHAR(ASCVAL)
            END IF
 50      CONTINUE
      ELSE
         DO 100 I = 1, ILEN
            IF ( (ICHAR(STRING(I:I)).GE.65) .AND.
     :           (ICHAR(STRING(I:I)).LE.90) ) THEN
               ASCVAL = ICHAR(STRING(I:I)) + 32
               STRING(I:I) = CHAR(ASCVAL)
            END IF
 100     CONTINUE
      END IF

      RETURN
      END
