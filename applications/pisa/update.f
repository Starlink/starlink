      SUBROUTINE UPDATE(IAP,T,THRESH,CONST,OFFSET)
C     *** accumulate areal profiles

C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK.

C     .. Scalar Arguments ..
      REAL CONST,OFFSET,T,THRESH
C     ..
C     .. Array Arguments ..
      INTEGER*4 IAP(8)
C     ..
C     .. Local Scalars ..
      REAL TEMP
      INTEGER I,NUP
C     ..
      TEMP = T - THRESH
      IF (TEMP.GE.0.5) THEN
         NUP = MIN0(8,INT(ALOG(TEMP+0.5)*CONST-OFFSET)+1)
         NUP = MAX0(1,NUP)
         DO 10 I = 1,NUP
            IAP(I) = IAP(I) + 1
 10      CONTINUE
      ENDIF

      END
