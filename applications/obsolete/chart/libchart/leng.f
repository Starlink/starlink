      INTEGER FUNCTION LENG(STRING)
*+
*   LENG Returns the Length of the String,Excluding any
*   Trailing Spaces.
*
*   Gets
*   ----
*      STRING  - The Character String in Question
*
*   Returns
*   -------
*      The actual length of the Character String
*
*  History:
*    15-MAR-1993 (AJJB):
*      Removed this function from file IIDATE and put it in it's own
*      file.


*-
      CHARACTER*(*) STRING
      INTEGER LENGTH

      LENGTH=LEN(STRING)
      DO K=LENGTH,1,-1
         IF (STRING(K:K).NE.' ') GOTO 100
      ENDDO
100   CONTINUE
      LENG=K
      END

