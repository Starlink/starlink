C*GRFAO - format character string containing integers
C+
      SUBROUTINE GRFAO (FORMAT, L, STR, V1, V2, V3, V4)
      CHARACTER*(*) FORMAT
      INTEGER L
      CHARACTER*(*) STR
      INTEGER V1, V2, V3, V4
C
C The input string FORMAT is copied to the output string STR with
C the first occurrence of '#' replaced by the value of V1, the second
C by the value of V2, etc.  The length of the resulting string is 
C returned in L.
C-----------------------------------------------------------------------
      INTEGER I,Q,VAL,GRITOC
C
      L = 0
      Q = 0
      DO 10 I=1,LEN(FORMAT)
          IF (L.GE.LEN(STR)) RETURN
          IF (FORMAT(I:I).NE.'#') THEN
              L = L+1
              STR(L:L) = FORMAT(I:I)
          ELSE
              Q = Q+1
              VAL = 0
              IF (Q.EQ.1) VAL = V1
              IF (Q.EQ.2) VAL = V2
              IF (Q.EQ.3) VAL = V3
              IF (Q.EQ.4) VAL = V4
              L = L + GRITOC(VAL, STR(L+1:))
          END IF
   10 CONTINUE
C-----------------------------------------------------------------------
      END
