C+
      INTEGER FUNCTION ICH_NTOCS(STRING,ARRAY,NBYTES)
C
C     I C H _ N T O C S
C
C     Numeric TO Character String
C
C     Converts a numeric array containing characters (at 1 byte
C     per character) into a character string.
C
C     Parameters -     (">" input, "<" output)
C
C     (<) STRING    (Character) Returned with the converted
C                   characters from ARRAY.
C     (>) ARRAY     (Numeric) Contains the characters to be
C                   converted.
C     (>) NBYTES    (Integer) The number of bytes in ARRAY.
C
C     Returns - (if called as a function)
C
C     (<) ICH_NTOCS (Integer) The number of the last non-blank
C                   character copied into the character string.
C                   In some circumstances this is the logical
C                   length of the string. The first character
C                   is number 1, not 0.
C
C     Note -
C
C     If STRING is longer than NBYTES, it will be blank filled
C     on the right.  If it is shorter, some characters will be
C     lost.
C
C                                          KS / UCL  7th June 1982
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
         CHR=CHAR(ARRAY(I))
         STRING(I:I)=CHR
         IF (CHR.NE.' ') THEN
            LNGTH=I
         END IF
      END DO
C
      IF (NBYTES.LT.LBYTES) THEN
         STRING(LBYTES+1:)=' '
      END IF
C
      ICH_NTOCS=LNGTH
C
      END
