      SUBROUTINE JTY_FILTER(N,K1,K2,K3,K4,X)
* Filter a Fourier tranform by multiplying by a cosine bell
      COMPLEX*8 X(1)

      DO J = 1,N
          IF(J.LT.N/2+1) THEN
              NUMBER = J - 1
          ELSE
              NUMBER = J - N - 1
          END IF
          NUMA = ABS(NUMBER)
          IF(NUMA.LT.K1.OR.NUMA.GT.K4) THEN
              FACTOR = 0
          ELSE IF(NUMA.LT.K2) THEN
              ARG = 3.14159 * FLOAT(NUMA-K1)/FLOAT(K2-K1)
              FACTOR = .5 * (1-COS(ARG))
          ELSE IF(NUMA.GT.K3) THEN
              ARG = 3.14159 * FLOAT(NUMA-K3)/FLOAT(K4-K3)
              FACTOR = .5 * (1+COS(ARG))
          ELSE
              FACTOR = 1
          END IF
          X(J) = X(J) * FACTOR
      END DO
      RETURN
      END
