      SUBROUTINE JTY_FLIP(N,BUF)
      REAL*4 BUF(1)
      DO J = 1,N/2
          TEMP = BUF(J)
          BUF(J) = BUF(J+N/2)
          BUF(J+N/2) = TEMP
      END DO
      RETURN
      END
