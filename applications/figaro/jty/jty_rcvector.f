      SUBROUTINE JTY_RCVECTOR(N,BUF,X)
* copy a real buffer to a complex one
      REAL*4 BUF(1)
      COMPLEX*8 X(1)
      DO J = N,1,-1
          X(J) = CMPLX(BUF(J))
      END DO
      RETURN
      END
