      FUNCTION JTY_LPI(I,Z)
* Legendre polynomial of order i-1
      REAL*8 JTY_LPI
      INCLUDE 'JTY_LEGENDRE'
      GOTO(1,2,3,4,5,6,7,8) I
1     JTY_LPI = LP0(Z)
      RETURN
2     JTY_LPI = LP1(Z)
      RETURN
3     JTY_LPI = LP2(Z)
      RETURN
4     JTY_LPI = LP3(Z)
      RETURN
5     JTY_LPI = LP4(Z)
      RETURN
6     JTY_LPI = LP5(Z)
      RETURN
7     JTY_LPI = LP6(Z)
      RETURN
8     JTY_LPI = LP7(Z)
      RETURN
      END
