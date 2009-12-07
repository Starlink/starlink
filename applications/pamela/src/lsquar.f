      SUBROUTINE LSQUAR(DATA,NUMBER,N,A,CHISQ,XM,NORM)
C
C   LQSUAR PROVIDES A POLYNOMIAL FIT OF SPECIFIED DEGREE TO A SET OF
C   DATA POINTS. THE FIT IS DONE IN DOUBLE PRECISION.
C   USES THE DOUBLE PRECISION MATRIX INVERSION SUBROUTINE 'MLSRAR'.
C
C   DATA    =  ARRAY CONTAINING THE DATA. THIS SHOULD BE ARRANGED:
C              DATA(1,K): X-VALUE OF DATA POINT.
C              DATA(2,K): Y-VALUE OF DATA POINT.
C              DATA(3,K): SIGMA IN Y-VALUE OF DATA POINT.
C              (REAL*8).
C   NUMBER  =  NUMBER OF DATA POINTS TO BE FITTED.
C   N       =  NUMBER OF COEFFICIENTS IN POLYNOMIAL TO BE FITTED. THE
C              ORDER OF THE POLYNOMIAL IS (N-1).
C   A       =  ARRAY HOLDING COEFFICIENTS OF FIT (REAL*8). THIS IS ARRANGED:
C              POLY = A(1) + A(2)*X + ... A(N)*X**(N-1)
C   CHISQ   =  CONTAINS THE CHI-SQUARE VALUE OF THE FIT ON OUTPUT. IF
C              CHISQ = -1. ON OUTPUT THEN THE MATRIX WAS SINGULAR, IF
C              CHISQ = -2. ON OUTPUT THEN OVERFLOW OR DIVIDE CHECK OCCURED.
C              CHISQ = -3. ON OUTPUT THEN INVALID PARAMETERS INPUT.
C              IF YOU SET CHISQ = 0. ON INPUT ERROR MESSAGES WILL BE PRINTED.
C              (REAL*8)
C   XM      =  WORKING STORAGE ARRAY (REAL*8). DIMENSION IN THE MAIN
C              PROGRAM AS XM(NMAX,2*NMAX+3) WHERE 'NMAX' IS THE MAXIMUM
C              VALUE OF 'N' YOU WISH TO CALL.
C   NORM    =  SCALING PARAMETER. SINCE LARGE POWERS OF THE INPUT DATA ARE
C              TO BE TAKEN OVERFLOWS OR UNDERFLOWS CAN EASILY OCCUR. IF
C              YOU SET NORM = 1 ON INPUT DATA ARE SCALED TO REDUCE LIKELIHOOD
C              OF THIS EVENT. NORM = 0 INSTRUCTS FOR NO SCALING. INPUT DATA
C              ARE NOT DESTROYED BY THE SCALING, AND COEFFICIENTS ARE
C              AUTOMATICALLY SCALED BACK BEFORE OUTPUT.
C
        LOGICAL RITE
        REAL*8 XM(N,2*N+3),XX,RR,EPS,AVE,SIGMA
        REAL*8 DATA(3,NUMBER),A(N),CHISQ,S,R
        REAL*8 X1, X2, X3
C
C   EPS    =  MAXIMUM ALLOWED ERROR IN ITERATIONS ON THE POLYNOMIAL
C             COEFFICIENTS. CONVERGENCE TERMINATES WHEN ERROR.LT.EPS
C   NMAX   =  MAXIMUM ALLOWED NUMBER OF COEFFICIENTS (DUE TO DIMENSION
C             STATEMENTS).
C
      DATA EPS,NMAX/1.D-6,50/
      RITE=.FALSE.
C
C   TEST INPUT DATA.
C
        IF((NUMBER.LE.0.OR.N.GT.NUMBER).OR.N.LE.0) THEN
          WRITE(6,801) NUMBER,N
801       FORMAT ('0LSQUAR: NO OF POINTS: ',I5,
     &    ' NO OF COEFFICIENTS: ',I5/' INVALID VALUES'//
     &    ' (MUST BE POSITIVE AND POINTS.GE.COEFFICIENTS)')
          CHISQ=-3.
          RETURN
        ELSE IF (N.GT.NMAX) THEN
          WRITE (*,802) N,NMAX
802       FORMAT ('0LSQUAR: NO OF COEFFICIENTS: ',
     &    I4,' MUST NOT EXCEED:',I4)
          CHISQ=-3.
          RETURN
        END IF
C
C   IF THE INPUT VALUE OF CHISQ IS 0. ALLOW PRINTING OF ERROR MESSAGES.
C
        IF (CHISQ.EQ.0.) RITE=.TRUE.
        ITER=5
        IF (RITE) ITER=-ITER
        N21 = 2*N + 1
        N22 = N21 + 1
        N23 = N21 + 2
C
C   RESCALE THE INPUT DATA (FOR NORM.NE.0).
C
        IF (NORM.NE.0 .AND. N.NE.1) THEN
          AVE = 0.0
          SIGMA = 0.0
          DO I=1,NUMBER
            X1 = DATA(1,I)
            AVE   = AVE + X1
            SIGMA = SIGMA + X1*X1
          END DO
          AVE = AVE/NUMBER
          SIGMA = DSQRT(SIGMA/NUMBER-AVE*AVE)
        END IF
C
C   ZERO THE WORKING ARRAY.
C
        DO I = 1,N
          DO J = N21,N23
            XM(I,J) = 0.D0
          END DO
        END DO
        X2 = 0.D0
        X3 = 0.D0
C
C   COMPUTE THE MOMENTS OF THE DATA.
C ** Change by TRM @RGO 7/6/88. Ignore point
* if sigma error estimate less than or equal to zero
*
        M2 = 2*N
        DO I = 1, NUMBER
          IF(DATA(3,I).GT.0.) THEN
            RR = (1.D0/DATA(3,I))**2
            X2 = X2 + RR
            XX = DATA(2,I)*RR
            X3 = X3 + XX
            IF(N.NE.1) THEN
              X1 = DATA(1,I)
              DO J = 3, M2
                IF (NORM.EQ.0) THEN
                  RR = RR*X1
                ELSE
                  RR = RR*(X1-AVE)/SIGMA
                END IF
                IF(J.GT.N) THEN
                  XM(J-N,N22)=XM(J-N,N22)+RR
                ELSE
                  XM(J,N21)=XM(J,N21)+RR
                END IF
              END DO
              DO J = 2, N
                IF (NORM.EQ.0) THEN
                  XX = XX*X1
                ELSE
                  XX = XX*(X1-AVE)/SIGMA
                END IF
                XM(J,N23) = XM(J,N23) + XX
              END DO
            END IF
          END IF
        END DO
        XM(2,N21) = X2
        XM(1,N23) = X3
C
C   COMPUTE MATRIX FOR INVERSION.
C
        DO I = 1, N
          DO J = 1, N
            K = I + J
            IF(K.GT.N) THEN
              XM(I,J) = XM(K-N,N22)
            ELSE
              XM(I,J) = XM(K,N21)
            END IF
          END DO
        END DO
C
C   CALL DOUBLE PRECISION MATRIX INVERSION ROUTINE.
C
        IF(N.NE.1) THEN
          CALL MLSRAR(N,XM,XM(1,N23),ITER,EPS,A,ITEST,0,XM(1,N+1))
          IF(ITEST.GE.5) THEN
            CHISQ = - 2.0
            RETURN
          END IF
        ELSE
          XM(1,1) = 1.D0/XM(1,1)
          A(1) = XM(1,1)*XM(1,N23)
        END IF

C
C   COMPUTE CHI-SQUARE FOR RESULTING FIT.
C
        CHISQ=0.0
        DO I=1,NUMBER
          IF(DATA(3,I).GT.0.) THEN
            S = A(1)
            IF(N.NE.1) THEN
              R=1.
              X1 = DATA(1,I)
              DO J = 2, N
                IF (NORM.EQ.0) THEN
                  R=R*X1
                ELSE
                  R=R*(X1-AVE)/SIGMA
                END IF
                S = S + A(J)*R
              END DO
            END IF
            CHISQ = CHISQ +((S-DATA(2,I))/DATA(3,I))**2
          END IF
        END DO
C
C   ERROR MESSAGES AFTER INVERSION OF THE MATRIX XM (H IN THE WRITE-UP).
C
        IF (NORM.EQ.0 .OR. N.EQ.1) RETURN
C
C   RESCALE COEFFICIENTS IF DATA SCALING WAS REQUESTED.
C
        SIGMA = 1.D0/SIGMA
        AVE = -AVE*SIGMA
        L = N-1
        DO I=1,L
          XM(I,1) = AVE**I
          XM(I,2) = 0.D0
        END DO
        XM(1,2) = 1.D0
        XM(N,2) = 0.D0
        DO I=1,L
          K=N-I+1
          DO J=2,K
            XM(J,2) = XM(J,2) + XM(J-1,2)
          END DO
          K = I+1
          DO J = K,N
            A(I) = A(I) + A(J)*XM(J-I+1,2)*XM(J-I,1)
          END DO
          A(I) = A(I)*SIGMA**(I-1)
        END DO
        A(N) = A(N)*SIGMA**(N-1)
        RETURN
        END

      SUBROUTINE MLSRAR(N,BDMTX,V,ITER,EPS,F,IT,INEW,A)
*
*   DOUBLE PRECISION matrix inversion.
*
*   N       =  Order of matrix.
*   BDMTX   =  Two-dimensional array of coefficients.
*   V       =  Right-hand vector.
*   ITER    =  Maximum number of iterations desired.
*   EPS     =  Tolerance for convergence.
*   F       =  Resulting vector.
*   IT      =  Output from routine specifying number of iterations actually
*              done.
*   INEW    =  Variable set to value .NE.1 on first call. On subsequent calls
*              it is set to 1 if the matrix is unchanged but the column
*              vector 'B' is changed.
*
      IMPLICIT NONE
      LOGICAL RITE
      INTEGER N, ITER, IT, I, J, N1, INEW, IM1, JMX
      INTEGER K, II, I2
      DOUBLE PRECISION BDMTX(N,N),V(N),F(N),A(N,N),X(50)
      DOUBLE PRECISION IDX(50),XT(50)
      DOUBLE PRECISION SG1,AMX, XI, CX, SUM, SING, EPS, R
      DOUBLE PRECISION T, ABSA
*
      RITE=.FALSE.
      IF (ITER.LT.0) RITE=.TRUE.
      ITER=IABS(ITER)
      IT = 0
      DO I=1,N
        X(I) = V(I)
        F(I) = 0.0
      END DO
      N1 = N-1
      IF(INEW.EQ.1) GOTO 181
      DO I=1,N
        DO J=1,N
          A(I,J)=BDMTX(I,J)
        END DO
      END DO
      DO I=1,N
        IDX(I)=I
      END DO
      SG1 = 0.
      DO I = 2, N
*
*   Partial pivoting, check for MAX element in (I-1)st column.
*
        IM1=I-1
        AMX=DABS(A(IM1,IM1))
        JMX=IM1
        DO J = I, N
          ABSA=DABS(A(J,IM1))
          IF(AMX.LT.ABSA) THEN
            AMX = ABSA
            JMX = J
          END IF
        END DO
        IF(JMX.NE.IM1) THEN
*
* Move the row with MAX A(J,IM1) to (IM1)st row.
*
          DO K = 1, N
            T = A(IM1,K)
            A(IM1,K) = A(JMX,K)
            A(JMX,K) = T
          END DO
          II = INT(IDX(IM1))
          IDX(IM1) = IDX(JMX)
          IDX(JMX) = II
          XI = X(IM1)
          X(IM1) = X(JMX)
          X(JMX) = XI
          SG1=1.0
        END IF
        IF(A(IM1,IM1).EQ.0.) GOTO 200
        DO J=I,N
          CX=A(J,IM1)/A(IM1,IM1)
          DO K=I,N
            A(J,K)=A(J,K)-CX*A(IM1,K)
          END DO
          A(J,IM1) = CX
        END DO
      END DO
*
*   Forward pass - operate on right hand side as on matrix.
*
62    CONTINUE
      DO I=2,N
        DO J=I,N
          X(J)=X(J)-X(I-1)*A(J,I-1)
        END DO
      END DO
*
*   Backward pass - solve for AX = B.
*
      X(N) = X(N)/A(N,N)
      DO I=1,N1
        SUM=0.0
        I2=N-I+1
        IM1=I2-1
        DO J=I2,N
          SUM=SUM+A(IM1,J)*X(J)
        END DO
        X(IM1)=(X(IM1)-SUM)/A(IM1,IM1)
      END DO
      DO I=1,N
        F(I) = F(I) + X(I)
      END DO
      SING = 0.
      IF (IT.EQ.ITER) RETURN
      IT = IT + 1
      DO I=1,N
        IF(F(I).EQ.0.) THEN
          SING = 1.0D38
          GOTO 150
        END IF
        SING = MAX(SING,ABS(X(I)/F(I)))
      END DO
      IF (SING.GT.EPS) GOTO 150
*
*   Finished.
*
      RETURN
*
*   DOUBLE PRECISION matrix multiplication.
*
150   CONTINUE
      DO I=1,N
        R=0.0D0
        DO J=1,N
          R=R+BDMTX(I,J)*F(J)
        END DO
        X(I)=V(I)-R
      END DO
181   IF(SG1.EQ.0.) GOTO 62
*
*   If SG1.NE.0, permute X before performing forward pass.
*
      DO I=1,N
        XT(I)=X(I)
      END DO
      DO I=1,N
        K=INT(IDX(I))
        X(I)=XT(K)
      END DO
      GOTO 62
200   IF (RITE) WRITE (*,510) IM1
510   FORMAT ('0MLSRAR: DIAGONAL TERM:',I3,' REDUCED TO ZERO')
      RETURN
      END
