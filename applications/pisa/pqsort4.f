      SUBROUTINE PQSORT4(X,POINT,L,NFILT)

C  Changes:
C     Changed name to pqsort from qsort to resolve naming clash
C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK. References to INTEGER*2 changed to
C        INTEGER *4

C     .. Scalar Arguments ..
      INTEGER L,NFILT
C     ..
C     .. Array Arguments ..
      REAL*4 X(NFILT)
      INTEGER*4 POINT(NFILT)
C     ..
C     .. Local Scalars ..
      REAL TEMP,TEST
      INTEGER I,II,J,NPT
      INTEGER*4 IT
C     ..

      TEST = X(L)
      DO 10 I = 1,NFILT
         IF (I.NE.L) THEN
            IF (TEST.LE.X(I)) GOTO 20
         ENDIF
 10   CONTINUE
      J = NFILT + 1
      GOTO 30
 20   J = I
 30   IF (J-1.NE.L) THEN
         IF (J-L) 40,50,60
 40      TEMP = X(L)
         IT = POINT(L)
         NPT = L - J
         DO 70 I = 1,NPT
            II = L - I
            X(II+1) = X(II)
            POINT(II+1) = POINT(II)
 70      CONTINUE
         X(J) = TEMP
         POINT(J) = IT
         RETURN
 60      TEMP = X(L)
         IT = POINT(L)
         J = J - 1
         NPT = J - L
         IF (NPT.NE.0) THEN
            DO 80 I = 1,NPT
               II = L + I
               X(II-1) = X(II)
               POINT(II-1) = POINT(II)
 80         CONTINUE
         ENDIF
         X(J) = TEMP
         POINT(J) = IT
 50      RETURN
      ENDIF
      END
