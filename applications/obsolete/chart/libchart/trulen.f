      INTEGER FUNCTION TRULEN(STRING)
*+
*         FUNCTION TRULEN
*
*   It gives the actual length of a  string,  ignoring  any  trailing
*   spaces  and  null  characters. It should be noted that the result
*   returned in TRULEN as the value of this function  is  an  INTEGER
*   and must be typed as such in the calling segment.
*
*   Gets
*   ----
*      STRING    CHAR     - This is the  text  string,  of  arbitrary
*                           length, whose length is to be found
*
*   Returns
*   -------
*      TRULEN    Integer  - The length of the string.
*
*   Written by K F Hartley at RGO on 8/12/81
*   K F Hartley RGO 11-JUN-82
*-
      CHARACTER*(*) STRING
      CHARACTER*1 NULL
      DATA NULL/'0'X/
*
*   It is assumed that STRING is full of a sequence of significant
*   characters, followed by a sequence of spaces and/or null characters
*
      L=LEN(STRING)
*
*   L now holds the total length of STRING
*
  100 CONTINUE
      IF (L.GT.0) THEN
         IF (STRING(L:L).EQ.' '.OR.STRING(L:L).EQ.NULL) THEN
            L=L-1
            GO TO 100
         END IF
         TRULEN=L
      ELSE
         TRULEN=0
      END IF
      END
