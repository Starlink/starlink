      SUBROUTINE PDA_QRUPDD(NR,N,A,U,V)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C FIND AN ORTHOGONAL (N*N) MATRIX (Q*) AND AN UPPER TRIANGULAR (N*N)
C MATRIX (R*) SUCH THAT (Q*)(R*)=R+U(V+)
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(N,N)      <--> ON INPUT:  CONTAINS R
C                  ON OUTPUT: CONTAINS (R*)
C U(N)         --> VECTOR
C V(N)         --> VECTOR
C
      DIMENSION A(NR,1)
      DIMENSION U(N),V(N)
C
C DETERMINE LAST NON-ZERO IN U(.)
C
      K=N
   10 IF(U(K).NE.0.D0 .OR. K.EQ.1) GO TO 20
C     IF(U(K).EQ.0.D0 .AND. K.GT.1)
C     THEN
        K=K-1
        GO TO 10
C     ENDIF
C
C (K-1) JACOBI ROTATIONS TRANSFORM
C     R + U(V+) --> (R*) + (U(1)*E1)(V+)
C WHICH IS UPPER HESSENBERG
C
   20 IF(K.LE.1) GO TO 40
        KM1=K-1
        DO 30 II=1,KM1
          I=KM1-II+1
          IF(U(I).NE.0.D0) GO TO 25
C         IF(U(I).EQ.0.)
C         THEN
            CALL PDA_QRAX1D(NR,N,A,I)
            U(I)=U(I+1)
            GO TO 30
C         ELSE
   25       CALL PDA_QRAX2D(NR,N,A,I,U(I),-U(I+1))
            U(I)=SQRT(U(I)*U(I) + U(I+1)*U(I+1))
C         ENDIF
   30   CONTINUE
C     ENDIF
C
C R <-- R + (U(1)*E1)(V+)
C
   40 DO 50 J=1,N
        A(1,J)=A(1,J) +U(1)*V(J)
   50 CONTINUE
C
C (K-1) JACOBI ROTATIONS TRANSFORM UPPER HESSENBERG R
C TO UPPER TRIANGULAR (R*)
C
      IF(K.LE.1) GO TO 100
        KM1=K-1
        DO 80 I=1,KM1
          IF(A(I,I).NE.0.D0) GO TO 70
C         IF(A(I,I).EQ.0.)
C         THEN
            CALL PDA_QRAX1D(NR,N,A,I)
            GO TO 80
C         ELSE
   70       T1=A(I,I)
            T2=-A(I+1,I)
            CALL PDA_QRAX2D(NR,N,A,I,T1,T2)
C         ENDIF
   80   CONTINUE
C     ENDIF
  100 RETURN
      END
