      LOGICAL FUNCTION SET(L,N)
*+
*   True if Bit N (1-8) of L is Set
*
*-
      BYTE L
      INTEGER N,I
      SET = .FALSE.
      IF (N.LT.1.OR.N.GT.8) RETURN
      IF (L.LT.0) THEN
        I = L+256
      ELSE
        I = L
      ENDIF
      SET = MOD(I/(2**(8-N)),2).EQ.1
      END


