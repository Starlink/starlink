      SUBROUTINE JTY_CORRELATE(N,FT1,FT2,CFN)
      COMPLEX*8 FT1(1), FT2(1), CFN(1)
      DO J = 1,N
          CFN(J) = FT1(J) * CONJG(FT2(J))
      END DO
      RETURN
      END
