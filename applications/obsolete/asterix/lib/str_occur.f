*+  STR_OCCUR - returns number of occurences of A in B
      INTEGER FUNCTION STR_OCCUR(A,B)
*    Description :
      CHARACTER*(*) A,B
*
*
      LOGICAL FINI
      INTEGER C1,C2,CA
      INTEGER LENA,LENB

*
*
      INTEGER CHR_LEN
*-
      LENA=CHR_LEN(A)
      LENB=CHR_LEN(B)

      C1=1
      C2=LENB

      N=0
      FINI=.FALSE.
      DO WHILE (.NOT.FINI)
        CA=INDEX(B(C1:C2),A)
        IF (CA.NE.0) THEN
          N=N+1
          C1=CA+LENA
          C1=MIN(C1,C2)
        ELSE
          FINI=.TRUE.
        ENDIF
      ENDDO

      STR_OCCUR=N

      END
