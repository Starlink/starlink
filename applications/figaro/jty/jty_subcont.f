      SUBROUTINE JTY_SUBCONT(NPT,DATA,CURVE)
* Program to fit and subtract the continuum from a spectrum

      REAL*4 DATA(NPT), CURVE(NPT)
C     REAL*4 TEMP(NPT)
      REAL*4 JTY_LPOLY, SCALE(2)
      REAL*8 COEFF(5)

      DO I = 1,NPT
          CURVE(I) = I
      END DO
* Include this for division
*     DO 5 J = 1,NPT
*5    TEMP(J) = ALOG(DATA(J))
      SCALE(1) = CURVE(1)
      SCALE(2) = CURVE(NPT)
      NCOEFF = 5
      CALL JTY_FITLPOLY(NPT,CURVE,DATA,SCALE,NCOEFF,COEFF)

      DO 70 J = 1,NPT
      CURVE(J) = JTY_LPOLY(CURVE(J),SCALE,NCOEFF,COEFF)
* Include this for division
*     CURVE(J) = EXP(CURVE(J))

* This is the original fit subtraction
70    DATA(J) = DATA(J) - CURVE(J)

* This is a test to see if division works better
*     if(curve(j).le.0) write(6,*) 'help, help: continuum fit < 0'
*70   DATA(J) = DATA(J)/CURVE(J) - 1

      RETURN
      END
