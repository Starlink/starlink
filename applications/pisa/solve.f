      SUBROUTINE SOLVE(A,B,M)
c     *** gauss elimination to solve ax=b

C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK.

C     .. Scalar Arguments ..
      INTEGER M
C     ..
C     .. Array Arguments ..
      REAL A(25,25),B(25)
C     ..
C     .. Local Scalars ..
      REAL BIG,PIVOT,RMAX,TEMP
      INTEGER I,IB,IR,IU,J,JL,K,L
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..

      IU = M - 1
      DO 10 I = 1,IU

c     *** find largest remaining term in ith column for pivot
         BIG = 0.0
         DO 20 K = I,M
            RMAX = ABS(A(K,I))
            IF (RMAX-BIG.GT.0) THEN
               BIG = RMAX
               L = K
            ENDIF
 20      CONTINUE

c     *** check for non-zero term
         IF (BIG.EQ.0.0) THEN
            GOTO 30
         ELSE
            IF (I.NE.L) THEN

c     *** switch rows
               DO 40 J = 1,M
                  TEMP = A(I,J)
                  A(I,J) = A(L,J)
                  A(L,J) = TEMP
 40            CONTINUE
               TEMP = B(I)
               B(I) = B(L)
               B(L) = TEMP
            ENDIF

c     *** pivotal reduction
            PIVOT = A(I,I)
            JL = I + 1
            DO 50 J = JL,M
               TEMP = A(J,I)/PIVOT
               B(J) = B(J) - TEMP*B(I)
               DO 60 K = I,M
                  A(J,K) = A(J,K) - TEMP*A(I,K)
 60            CONTINUE
 50         CONTINUE
         ENDIF
 10   CONTINUE

c     *** back substitution for solution
      DO 70 I = 1,M
         IR = M + 1 - I
         IF (A(IR,IR).EQ.0.0) THEN

            B(IR) = 0.0
         ELSE
            TEMP = B(IR)
            IF (IR.NE.M) THEN
               DO 80 J = 2,I
                  K = M + 2 - J
                  TEMP = TEMP - A(IR,K)*B(K)
 80            CONTINUE
            ENDIF
            B(IR) = TEMP/A(IR,IR)
         ENDIF
 70   CONTINUE
      RETURN

 30   DO 90 IB = 1,M
         B(IB) = 0.0
 90   CONTINUE
      END
