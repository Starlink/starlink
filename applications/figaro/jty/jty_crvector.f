      SUBROUTINE JTY_CRVECTOR(N,X,BUF)
* copy a complex buffer to a real one
      REAL*4 BUF(1)
      COMPLEX*8 X(1)
      DO J = 1,N
          BUF(J) = REAL(X(J))
      END DO
      RETURN
      END
