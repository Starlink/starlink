*
* LUDCMP -- does LU decomposition of matrix A(N,N)
* A is returned with upper and lower triangle stored
* See NR for more detail. STATUS.NE.0 indicates failure.
*
      SUBROUTINE LUDCMP(A,N,NP,INDX,D,STATUS)
      IMPLICIT NONE
      INTEGER NMAX, N, NP, I, IMAX, J, K, STATUS
      DOUBLE PRECISION TINY
      PARAMETER (NMAX=400,TINY=1.D-20)
      DOUBLE PRECISION A(NP,NP), VV(NMAX), D, AAMAX, SUM
      DOUBLE PRECISION DUM
      INTEGER INDX(N)
C
      D = 1.D0
      DO I = 1, N
         AAMAX = 0.D0
         DO J = 1, N
            IF(ABS(A(I,J)).GT.AAMAX) AAMAX = ABS(A(I,J))
         END DO
         IF(AAMAX.EQ.0.D0) THEN
            STATUS = 1
            RETURN
         END IF
         VV(I) = 1.D0/AAMAX
      END DO
      DO J = 1, N
         IF(J.GT.1) THEN
            DO I = 1, J-1
               SUM=A(I,J)
               IF(I.GT.1) THEN
                  DO K = 1, I-1
                     SUM = SUM-A(I,K)*A(K,J)
                  END DO
                  A(I,J)=SUM
               END IF
            END DO
         END IF
         AAMAX = 0.D0
         DO I=J,N
            SUM=A(I,J)
            IF(J.GT.1) THEN
               DO K=1,J-1
                  SUM=SUM-A(I,K)*A(K,J)
               END DO
               A(I,J)=SUM
            END IF
            DUM=VV(I)*ABS(SUM)
            IF(DUM.GE.AAMAX) THEN
               IMAX=I
               AAMAX=DUM
            END IF
         END DO
         IF(J.NE.IMAX) THEN
            DO K=1,N
               DUM=A(IMAX,K)
               A(IMAX,K)=A(J,K)
               A(J,K)=DUM
            END DO
            D=-D
            VV(IMAX)=VV(J)
         END IF
         INDX(J)=IMAX
         IF(J.NE.N) THEN
            IF(A(J,J).EQ.0.D0) A(J,J)=TINY
            DUM=1.D0/A(J,J)
            DO I=J+1,N
               A(I,J)=A(I,J)*DUM
            END DO
         END IF
      END DO
      IF(A(N,N).EQ.0.D0) A(N,N)=TINY
      STATUS=0
      RETURN
      END

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
      IMPLICIT NONE
      INTEGER N,NP,INDX(N),II,I,J,LL
      DOUBLE PRECISION A(NP,NP),B(N),SUM
C
      II=0
      DO I = 1, N
         LL=INDX(I)
         SUM=B(LL)
         B(LL)=B(I)
         IF(II.NE.0) THEN
            DO J=II,I-1
               SUM=SUM-A(I,J)*B(J)
            END DO
         ELSE IF(SUM.NE.0.D0) THEN
            II=I
         END IF
         B(I)=SUM
      END DO
      DO I=N,1,-1
         SUM=B(I)
         IF(I.LT.N) THEN
            DO J=I+1,N
               SUM=SUM-A(I,J)*B(J)
            END DO
         END IF
         B(I)=SUM/A(I,I)
      END DO
      RETURN
      END

