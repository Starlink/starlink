      SUBROUTINE JTY_CCVECTOR(N,X1,X2)
* Copy a complex buffer to another
      COMPLEX*8 X1(1),X2(1)
      DO J = 1,N
          X2(J) = X1(J)
      END DO
      RETURN
      END
