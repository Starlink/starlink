
C--------------------------------------------------------------------------

      INTEGER*4 FUNCTION GEN_KNTWRD(STRING)

C  Routine to count the number of words in string STRING, where the
C  words are defined to be seperated by hyphens.

      CHARACTER STRING*(*)

      GEN_KNTWRD=1
      DO 100 I=1,LEN(STRING)
      IF(STRING(I:I).EQ.'-') GEN_KNTWRD=GEN_KNTWRD+1
  100 CONTINUE
      RETURN
      END
