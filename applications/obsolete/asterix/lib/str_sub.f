*+  STR_SUB - returns .TRUE. if A is a substring of B
      LOGICAL FUNCTION STR_SUB(A,B)
*    Description :
*     returns a true value if the non-blank part of
*     A is a substring of the non-blank part of B
*     regardless of case
*
      IMPLICIT NONE

      CHARACTER*(*) A,B
*
*
      LOGICAL MATCH,SUB
      INTEGER I,J,K,L
      INTEGER LENA,LENB
      INTEGER IA,IB
*
*
      INTEGER CHR_LEN
*-
      LENA=CHR_LEN(A)
      LENB=CHR_LEN(B)

      IF (LENB.GE.LENA.AND.LENA.GT.0) THEN

        J=1
        K=LENB-LENA+1
        SUB=.FALSE.
        DO WHILE (J.LE.K.AND..NOT.SUB)
*  scan through to end or until a mismatch is found
          I=1
          MATCH=.TRUE.
          DO WHILE (MATCH.AND.I.LE.LENA)
            L=J+I-1

*  take care of different case
            IA=ICHAR(A(I:I))
            IB=ICHAR(B(L:L))
            IF (IA.GE.97.AND.IA.LE.121) THEN
              IA=IA-32
            ENDIF
            IF (IB.GE.97.AND.IB.LE.121) THEN
              IB=IB-32
            ENDIF

            MATCH=(IA.EQ.IB)

            I=I+1
          ENDDO

          SUB=MATCH
          J=J+1

        ENDDO

        STR_SUB=SUB

      ELSE
        STR_SUB=.FALSE.
      ENDIF

      END
