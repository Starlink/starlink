      SUBROUTINE DCHOLE4(A,B,N)

C     CHOLEsky decomposition of positive definite symmetric matrix
C     to solve Ax = b.

C  Changes:
C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK.
C        Note that this is called 4 as includes PSA1_PAR, this
C        makes it incompatable with normal PISAFIND routines.

C     ..Parameters..
      INCLUDE 'PSA1_PAR'        ! PISA parameters

C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Array Arguments ..
      REAL*8 A(IMNUM+1,IMNUM+1),B(IMNUM+1)
C     ..
C     .. Local Scalars ..
      REAL*8 SUM
      REAL AVEIGV,OFFSET
      INTEGER I,J,K
C     ..
C     .. Local Arrays ..
      REAL*8 L(IMNUM+1,IMNUM+1),Y(IMNUM+1)
C     ..

 10   CONTINUE
C     ..
      L(1,1) = DSQRT(A(1,1))
      DO 20 K = 2,N
         DO 30 J = 1,K - 1
            SUM = A(K,J)
            IF (J.NE.1) THEN
               DO 40 I = 1,J - 1
                  SUM = SUM - L(K,I)*L(J,I)
 40            CONTINUE
               L(K,J) = SUM/L(J,J)
            ENDIF

 30      CONTINUE
         SUM = A(K,K)
         DO 50 I = 1,K - 1
            SUM = SUM - L(K,I)*L(K,I)
 50      CONTINUE
         IF (SUM.LE.0.d0) THEN
            GOTO 60

         ELSE

            L(K,K) = DSQRT(SUM)
         ENDIF

 20   CONTINUE
      GOTO 70

 60   WRITE (*,FMT='(a)') ' **** warning matrix ill-conditioned ****'
      AVEIGV = A(1,1)
      DO 80 I = 2,N
         AVEIGV = AVEIGV + A(I,I)
 80   CONTINUE
C     *** max eigenvalue < trace
      OFFSET = AVEIGV*1.0e-15
      DO 90 I = 1,N
         A(I,I) = A(I,I) + OFFSET
 90   CONTINUE
      WRITE (*,FMT='(a)') ' Offset added to diagonal =',OFFSET
      GOTO 10
C     *** solve Ly = b
 70   Y(1) = B(1)/L(1,1)
      DO 100 I = 2,N
         SUM = B(I)
         DO 110 K = 1,I - 1
            SUM = SUM - L(I,K)*Y(K)
 110     CONTINUE
         Y(I) = SUM/L(I,I)
 100  CONTINUE
C     *** solve L(T)x = y
      B(N) = Y(N)/L(N,N)
      DO 120 I = N - 1,1,-1
         SUM = Y(I)
         DO 130 K = I + 1,N
            SUM = SUM - L(K,I)*B(K)
 130     CONTINUE
         B(I) = SUM/L(I,I)
 120  CONTINUE
      END
