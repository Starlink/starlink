      SUBROUTINE PARBOL(A,N,XPEAK,SD)
C     *** least-squares parabolic curve fitting

C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK.

C     .. Scalar Arguments ..
      REAL SD,XPEAK
      INTEGER N
C     ..
C     .. Array Arguments ..
      REAL A(N)
C     ..
C     .. Local Scalars ..
      REAL AA,FI,SDSQ,SUMA,SUMAY,SUMAYY,SUMY1,SUMY2,SUMY3,SUMY4
      INTEGER I,J
C     ..
C     .. Local Arrays ..
      REAL BVECT(25),CMATX(25,25)
C     ..
C     .. External Subroutines ..
      EXTERNAL SOLVE
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,FLOAT,SQRT
C     ..
      DO 10 I = 1,25
         BVECT(I) = 0.0
         DO 20 J = 1,25
            CMATX(I,J) = 0.0
 20      CONTINUE
 10   CONTINUE
      CMATX(1,1) = FLOAT(N)
      SUMA = 0.0
      SUMAY = 0.0
      SUMAYY = 0.0
      SUMY1 = 0.0
      SUMY2 = 0.0
      SUMY3 = 0.0
      SUMY4 = 0.0
      DO 30 I = 1,N
         AA = A(I)
         FI = FLOAT(I)
         SUMA = SUMA + AA
         SUMAY = SUMAY + AA*FI
         SUMAYY = SUMAYY + AA*FI**2
         SUMY1 = SUMY1 + FI
         SUMY2 = SUMY2 + FI**2
         SUMY3 = SUMY3 + FI**3
         SUMY4 = SUMY4 + FI**4
 30   CONTINUE
      CMATX(1,2) = SUMY1
      CMATX(1,3) = SUMY2
      CMATX(2,1) = SUMY1
      CMATX(2,2) = SUMY2
      CMATX(2,3) = SUMY3
      CMATX(3,1) = SUMY2
      CMATX(3,2) = SUMY3
      CMATX(3,3) = SUMY4
      BVECT(1) = SUMA
      BVECT(2) = SUMAY
      BVECT(3) = SUMAYY
      CALL SOLVE(CMATX,BVECT,3)
      IF (BVECT(3).EQ.0.0) THEN
         XPEAK = 0.0
         SD = 0.0

      ELSE

         XPEAK = -BVECT(2)/ (2.0*BVECT(3))
         SDSQ = 1.0/ (2.0*ABS(BVECT(3)))
         IF (SDSQ.LT.0.0) SDSQ = 0.0
         SD = SQRT(SDSQ)
      ENDIF

      END

