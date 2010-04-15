      SUBROUTINE SORTM4(IA,IB,N)

C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK. References to INTEGER*2 changed to
C        INTEGER *4

C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Array Arguments ..
      REAL*4 IA(N)
      INTEGER*4 IB(N)
C     ..
C     .. Local Scalars ..
      REAL*4 IT
      INTEGER I,IFIN,II,INT,J
      INTEGER*4 IU
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MIN0
C     ..
      INT = 2
 10   CONTINUE
      INT = 2*INT
      IF (INT.LT.N) GOTO 10
      INT = MIN0(N, (3*INT)/4-1)
 20   CONTINUE
      INT = INT/2
      IFIN = N - INT
      DO 30 II = 1,IFIN
         I = II
         J = I + INT
         IF (IA(I).GT.IA(J)) THEN
            IT = IA(J)
            IU = IB(J)
 40         CONTINUE
            IA(J) = IA(I)
            IB(J) = IB(I)
            J = I
            I = I - INT
            IF (I.GT.0) THEN
               IF (IA(I).GT.IT) GOTO 40
            ENDIF
            IA(J) = IT
            IB(J) = IU
         ENDIF
 30   CONTINUE
      IF (INT.GT.1) GOTO 20
      END

