      SUBROUTINE JTY_RRVECTOR(N,BUF1,BUF2)
* Copy a real buffer to another
      REAL*4 BUF1(1),BUF2(1)
      DO J = 1,N
          BUF2(J) = BUF1(J)
      END DO
      RETURN
      END
