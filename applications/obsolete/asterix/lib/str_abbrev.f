*+  STR_ABBREV - returns .TRUE. if A is an abbreviation of B
      LOGICAL FUNCTION STR_ABBREV(A,B)
*    Description :
*     returns a true value if the non-blank part of
*     A is a substring of the non-blank part of B
*     regardless of case
*
      CHARACTER*(*) A,B
*
*
      CHARACTER*1 AA,BB
      LOGICAL MATCH
      INTEGER I
      INTEGER LENA,LENB
*
*
      INTEGER CHR_LEN
*-
      LENA=CHR_LEN(A)
      LENB=CHR_LEN(B)

*  check lengths are compatible
      MATCH=(LENA.GT.0.AND.LENA.LE.LENB)
      I=1

*  scan through to end or until a mismatch is found
      DO WHILE (MATCH.AND.I.LE.LENA)
*  copy each character so as not to change original
        AA=A(I:I)
        BB=B(I:I)

*  check case and change if necessary
        IAA=ICHAR(AA)
        IBB=ICHAR(BB)
        IF (IAA.GE.97.AND.IAA.LE.121) THEN
          IAA=IAA-32
          AA=CHAR(IAA)
        ENDIF
        IF (IBB.GE.97.AND.IBB.LE.121) THEN
          IBB=IBB-32
          BB=CHAR(IBB)
        ENDIF

        MATCH=(AA.EQ.BB)
        I=I+1
      ENDDO

      STR_ABBREV=MATCH
      END
