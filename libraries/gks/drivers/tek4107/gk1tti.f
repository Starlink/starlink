

      SUBROUTINE GK1TTI(NUM,IBUFF,N)
      INCLUDE '../../include/check.inc'
      INTEGER IBUFF(*),NUM,N
      INTEGER HI1,HI2,LO
*
      LO=IABS(NUM)
      IF (LO.LT.16) GOTO 20
      HI1 = LO/16
      LO = LO-HI1*16
      IF (HI1.LT.64)GOTO 10
      HI2 = HI1/64
      HI1 = HI1-HI2*64
      IBUFF(N)=HI2+64
      N=N+1
10    IBUFF(N)=HI1+64
      N=N+1
20    IF(NUM.GE.0)LO=LO+16
      IBUFF(N)=LO+32
      N=N+1
      RETURN
      END
