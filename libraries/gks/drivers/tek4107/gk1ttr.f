

      SUBROUTINE GK1TTR(RNUM,MANT,EXP)
      INCLUDE '../../include/check.inc'
*
      REAL RNUM
      INTEGER MANT,EXP
*
*  LOCALS
*  ------
*
      REAL EPS
*
*       Set eps to 2 to the power -14
      EPS=0.00006104
      EXP=0
10    IF(RNUM .LE. AINT(RNUM) + EPS)GOTO 20
      RNUM=RNUM*2.0
      EXP=EXP-1
      GOTO 10
20    IF(RNUM .LT. 32767.0)GOTO 30
      RNUM=RNUM*0.5
      EXP=EXP+1
      GOTO 20
30    MANT=AINT(RNUM + 0.5)
      IF(RNUM .LT. 0.0)MANT=-MANT
      RETURN
      END
