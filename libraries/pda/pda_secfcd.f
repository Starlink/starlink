      SUBROUTINE PDA_SECFCD(NR,N,X,G,A,XPLS,GPLS,EPSM,ITNCNT,RNF,
     +     IAGFLG,NOUPDT,S,Y,U,W)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C UPDATE HESSIAN BY THE BFGS FACTORED METHOD
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> OLD ITERATE, X[K-1]
C G(N)         --> GRADIENT OR APPROXIMATE AT OLD ITERATE
C A(N,N)      <--> ON ENTRY: CHOLESKY DECOMPOSITION OF HESSIAN IN
C                    LOWER PART AND DIAGONAL.
C                  ON EXIT:  UPDATED CHOLESKY DECOMPOSITION OF HESSIAN
C                    IN LOWER TRIANGULAR PART AND DIAGONAL
C XPLS(N)      --> NEW ITERATE, X[K]
C GPLS(N)      --> GRADIENT OR APPROXIMATE AT NEW ITERATE
C EPSM         --> MACHINE EPSILON
C ITNCNT       --> ITERATION COUNT
C RNF          --> RELATIVE NOISE IN OPTIMIZATION FUNCTION FCN
C IAGFLG       --> =1 IF ANALYTIC GRADIENT SUPPLIED, =0 ITHERWISE
C NOUPDT      <--> BOOLEAN: NO UPDATE YET
C                  [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C S(N)         --> WORKSPACE
C Y(N)         --> WORKSPACE
C U(N)         --> WORKSPACE
C W(N)         --> WORKSPACE
C
      DIMENSION X(N),XPLS(N),G(N),GPLS(N)
      DIMENSION A(NR,1)
      DIMENSION S(N),Y(N),U(N),W(N)
      LOGICAL NOUPDT,SKPUPD
C
      IF(ITNCNT.EQ.1) NOUPDT=.TRUE.
      DO 10 I=1,N
        S(I)=XPLS(I)-X(I)
        Y(I)=GPLS(I)-G(I)
   10 CONTINUE
      DEN1=PDA_DDOT(N,S,1,Y,1)
      SNORM2=PDA_DNRM2(N,S,1)
      YNRM2=PDA_DNRM2(N,Y,1)
      IF(DEN1.LT.SQRT(EPSM)*SNORM2*YNRM2) GO TO 110
C     IF(DEN1.GE.SQRT(EPSM)*SNORM2*YNRM2)
C     THEN
        CALL PDA_MVMLUD(NR,N,A,S,U)
        DEN2=PDA_DDOT(N,U,1,U,1)
C
C       L <-- SQRT(DEN1/DEN2)*L
C
        ALP=SQRT(DEN1/DEN2)
        IF(.NOT.NOUPDT) GO TO 50
C       IF(NOUPDT)
C       THEN
          DO 30 J=1,N
            U(J)=ALP*U(J)
            DO 20 I=J,N
              A(I,J)=ALP*A(I,J)
   20       CONTINUE
   30     CONTINUE
          NOUPDT=.FALSE.
          DEN2=DEN1
          ALP=1.0D0
C       ENDIF
   50   SKPUPD=.TRUE.
C
C       W = L(L+)S = HS
C
        CALL PDA_MVMLLD(NR,N,A,U,W)
        I=1
        IF(IAGFLG.NE.0) GO TO 55
C       IF(IAGFLG.EQ.0)
C       THEN
          RELTOL=SQRT(RNF)
          GO TO 60
C       ELSE
   55     RELTOL=RNF
C       ENDIF
   60   IF(I.GT.N .OR. .NOT.SKPUPD) GO TO 70
C       IF(I.LE.N .AND. SKPUPD)
C       THEN
          IF(ABS(Y(I)-W(I)) .LT. RELTOL*MAX(ABS(G(I)),ABS(GPLS(I))))
     +         GO TO 65
C         IF(ABS(Y(I)-W(I)) .GE. RELTOL*AMAX1(ABS(G(I)),ABS(GPLS(I))))
C         THEN
            SKPUPD=.FALSE.
            GO TO 60
C         ELSE
   65       I=I+1
            GO TO 60
C         ENDIF
C       ENDIF
   70   IF(SKPUPD) GO TO 110
C       IF(.NOT.SKPUPD)
C       THEN
C
C         W=Y-ALP*L(L+)S
C
          DO 75 I=1,N
            W(I)=Y(I)-ALP*W(I)
   75     CONTINUE
C
C         ALP=1/SQRT(DEN1*DEN2)
C
          ALP=ALP/DEN1
C
C         U=(L+)/SQRT(DEN1*DEN2) = (L+)S/SQRT((Y+)S * (S+)L(L+)S)
C
          DO 80 I=1,N
            U(I)=ALP*U(I)
   80     CONTINUE
C
C         COPY L INTO UPPER TRIANGULAR PART.  ZERO L.
C
          IF(N.EQ.1) GO TO 93
          DO 90 I=2,N
            IM1=I-1
            DO 85 J=1,IM1
              A(J,I)=A(I,J)
              A(I,J)=0.D0
   85       CONTINUE
   90     CONTINUE
C
C         FIND Q, (L+) SUCH THAT  Q(L+) = (L+) + U(W+)
C
   93     CALL PDA_QRUPDD(NR,N,A,U,W)
C
C         UPPER TRIANGULAR PART AND DIAGONAL OF A NOW CONTAIN UPDATED
C         CHOLESKY DECOMPOSITION OF HESSIAN.  COPY BACK TO LOWER
C         TRIANGULAR PART.
C
          IF(N.EQ.1) GO TO 110
          DO 100 I=2,N
            IM1=I-1
            DO 95 J=1,IM1
              A(I,J)=A(J,I)
   95       CONTINUE
  100     CONTINUE
C       ENDIF
C     ENDIF
  110 RETURN
      END
