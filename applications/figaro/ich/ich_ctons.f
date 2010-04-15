C+
      INTEGER FUNCTION ICH_CTONS(STRING,ARRAY,NBYTES)
C
C     I C H _ C T O N S
C
C     Character TO Numeric String.
C
C     Converts a character string into a numeric 'character string'
C     at one byte per character.
C
C     Parameters -     (">" input, "<" output)
C
C     (>) STRING    (Character string) Contains the character
C                   string to be converted.
C     (<) ARRAY     (Numeric array) Returned containing the
C                   characters of STRING.
C     (>) NBYTES    (Integer) The number of bytes in ARRAY.
C
C     Returns (if called as a function) -
C
C     (<) ICH_CTONS (Integer) The number of the last non-blank
C                   character copied into ARRAY.  (In some
C                   conditions this is the logical length of
C                   the string.)
C
C     Note -
C
C     If the character string is longer than NBYTES it is
C     truncated.  If shorter, ARRAY is blank filled on the
C     right.
C
C                               KS / UCL   4th June 1982
C+
      INTEGER NBYTES
      CHARACTER*(*) STRING
      BYTE ARRAY(NBYTES)
C
      INTEGER I,LBYTES,LNGTH
      CHARACTER CHR
C
      LBYTES=MIN(LEN(STRING),NBYTES)
      LNGTH=0
      DO I=1,LBYTES
         CHR=STRING(I:I)
         ARRAY(I)=ICHAR(CHR)
         IF (CHR.NE.' ') THEN
            LNGTH=I
         END IF
      END DO
C
      IF (NBYTES.GT.LBYTES) THEN
         LBYTES=LBYTES+1
         DO I=LBYTES,NBYTES
            ARRAY(I)=ICHAR(' ')
         END DO
      END IF
C
      ICH_CTONS=LNGTH
C
      END
