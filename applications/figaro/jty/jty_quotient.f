      SUBROUTINE JTY_QUOTIENT(N,FT1,FT2,CFN)
      COMPLEX*8 FT1(1), FT2(1), CFN(1)
      DO J = 1,N
          IF(FT2(J).NE.0) THEN
              CFN(J) = FT1(J) / FT2(J)
          ELSE
              CFN(J) = 0
          END IF
      END DO
      RETURN
      END
