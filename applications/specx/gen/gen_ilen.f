
C-----------------------------------------------------------------

       INTEGER FUNCTION GEN_ILEN (STRING)
C      ----------------------------------

C  Returns the filled length of the string STRING

       CHARACTER  STRING*(*)

       L=LEN(STRING)
       DO I=1,L
         ILEN=L+1-I
         IF(STRING(ILEN:ILEN).NE.' ') GO TO 1
       ENDDO
       ILEN=0
    1  GEN_ILEN=ILEN

       END
