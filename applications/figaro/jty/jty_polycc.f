      SUBROUTINE JTY_POLYCC(NX,NY,SCALE,COEFF,PCOEFF)
* Routine to convert coefficients of a 2-d Legendre polynomial to
* coefficients of a plain polynomial
* The coefficients are stored as NX polynomials of order NY-1.
* Low order coefficient comes first, and the low order coefficient
* of the x polynomial is obtained by evaluating the first y polynomial.
      REAL*8 COEFF(1), PCOEFF(1), XSC(2), YSC(2), TEMP
      REAL*4 SCALE(4)
      INCLUDE 'JTY_LEGENDRE'

      XSC(1) = DBLE(SCALE(1))
      XSC(2) = DBLE(SCALE(2))
      YSC(1) = DBLE(SCALE(3))
      YSC(2) = DBLE(SCALE(4))

      DO 10 J = 0,NX-1
      DO 11 K = 0,NY-1
      TEMP = 0
      DO 12 I = K,NY-1
12    TEMP = TEMP + COEFF(J*NY+I+1) * LCOEFF(K+1,I+1)
11    PCOEFF(J*NY+K+1) = TEMP
      DO 13 K = 0,NY-1
      TEMP = 0
      DO 14 I = K,NY-1
14    TEMP = TEMP + PCOEFF(J*NY+I+1) * YSC(1)**I * (-YSC(2))**(I-K) *
     1DFLOAT(JTY_IBC(K,I))
13    PCOEFF(J*NY+K+1) = TEMP
10    CONTINUE

      DO 20 J = 0,NY-1
      DO 21 K = 0,NX-1
      TEMP = 0
      DO 22 I = K,NX-1
22    TEMP = TEMP + PCOEFF(I*NY+J+1) * LCOEFF(K+1,I+1)
21    PCOEFF(K*NY+J+1) = TEMP
      DO 23 K = 0,NX-1
      TEMP = 0
      DO 24 I = K,NX-1
24    TEMP = TEMP + PCOEFF(I*NY+J+1) * XSC(1)**I * (-XSC(2))**(I-K) *
     1DFLOAT(JTY_IBC(K,I))
23    PCOEFF(K*NY+J+1) = TEMP
20    CONTINUE

      RETURN
      END
