      SUBROUTINE POLYNM(XDAT,XCOR,N,POLYCF,M,ILIM)
C     *** least-squares fit of order m polynomial to n data points

C  Changes:
C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK.

C     .. Scalar Arguments ..
      INTEGER ILIM,M,N
C     ..
C     .. Array Arguments ..
      REAL POLYCF(M),XCOR(N),XDAT(N)
C     ..
C     .. Local Scalars ..
      REAL TEMP
      INTEGER I,J,JU,K
C     ..
C     .. Local Arrays ..
      REAL A(25,25),B(25)
C     ..
C     .. External Subroutines ..
      EXTERNAL SOLVE
C     ..

      IF (N.LT.M) THEN
         STOP ' too few data points'
      ELSEIF (M.GT.25) THEN
         STOP ' order of polynomial too large'
      ELSE

C     *** clear arrays
         DO 10 I = 1,25
            B(I) = 0.0
            DO 20 J = 1,25
               A(I,J) = 0.0
 20         CONTINUE
 10      CONTINUE

C     *** cumulate sums
         DO 30 I = 1,N
            DO 40 K = 1,M
               TEMP = 1.0
               IF (K+ILIM-1.NE.0) TEMP = XCOR(I)** (K-1+ILIM)
               B(K) = B(K) + XDAT(I)*TEMP
               DO 50 J = 1,K
                  TEMP = 1.0
                  IF (K+J-2+2*ILIM.NE.0) TEMP = XCOR(I)**
     +                 (K+J-2+2*ILIM)
                  A(K,J) = A(K,J) + TEMP
 50            CONTINUE
 40         CONTINUE
 30      CONTINUE
         DO 60 K = 2,M
            JU = K - 1
            DO 70 J = 1,JU
               A(J,K) = A(K,J)
 70         CONTINUE
 60      CONTINUE

C     *** solve linear equations
         CALL SOLVE(A,B,M)
         DO 80 I = 1,M
            POLYCF(I) = B(I)
 80      CONTINUE
      ENDIF

      END
