      REAL FUNCTION JTY_EVALEG(ORDER,X,SCALE,C)
      REAL*8 C(1)
      REAL*4 SCALE(2)
      INTEGER*4 ORDER
      INTEGER STATUS
* Evaluates sum ( C(i) * LPi(a*(x-b)) )
* Legendre polynomials 0 - 7
      INCLUDE 'JTY_LEGENDRE'

      JTY_EVALEG = 0.
      Z = SCALE(1) * (X - SCALE(2))
      NCOEFF = ORDER + 1
      IF(NCOEFF.LT.1.OR.NCOEFF.GT.8) THEN
          CALL PAR_WRUSER(' JTY_EVALEG: wrong number of coeffs',STATUS)
          RETURN
      ENDIF

      GOTO(1,2,3,4,5,6,7,8) NCOEFF
1     JTY_EVALEG = C(1)*LP0(Z)
      RETURN
2     JTY_EVALEG = C(1)*LP0(Z) + C(2)*LP1(Z)
      RETURN
3     JTY_EVALEG = C(1)*LP0(Z) + C(2)*LP1(Z) + C(3)*LP2(Z)
      RETURN
4     JTY_EVALEG = C(1)*LP0(Z) + C(2)*LP1(Z) + C(3)*LP2(Z) +
     1C(4)*LP3(Z)
      RETURN
5     JTY_EVALEG = C(1)*LP0(Z) + C(2)*LP1(Z) + C(3)*LP2(Z) +
     1C(4)*LP3(Z) + C(5)*LP4(Z)
      RETURN
6     JTY_EVALEG = C(1)*LP0(Z) + C(2)*LP1(Z) + C(3)*LP2(Z) +
     1C(4)*LP3(Z) + C(5)*LP4(Z) + C(6)*LP5(Z)
      RETURN
7     JTY_EVALEG = C(1)*LP0(Z) + C(2)*LP1(Z) + C(3)*LP2(Z) +
     1C(4)*LP3(Z) + C(5)*LP4(Z) + C(6)*LP5(Z) +
     1C(7)*LP6(Z)
      RETURN
8     JTY_EVALEG = C(1)*LP0(Z) + C(2)*LP1(Z) + C(3)*LP2(Z) +
     1C(4)*LP3(Z) + C(5)*LP4(Z) + C(6)*LP5(Z) +
     1C(7)*LP6(Z) + C(8)*LP7(Z)
      RETURN
      END
