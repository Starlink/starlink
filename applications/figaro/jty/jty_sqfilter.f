      SUBROUTINE JTY_SQFILTER(N,K1,K2,K3,K4,RMS,X,R)
* Filter a Fourier tranform by multiplying by a square root cosine bell
* Inverse transform, divide by RMS and put in R
      COMPLEX*8 X(1)
      REAL*4 R(1)

      LOG2N = NINT(ALOG(FLOAT(N))/ALOG(2.))
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
              FACTOR = SQRT(.5 * (1-COS(ARG)))
          ELSE IF(NUMA.GT.K3) THEN
              ARG = 3.14159 * FLOAT(NUMA-K3)/FLOAT(K4-K3)
              FACTOR = SQRT(.5 * (1+COS(ARG)))
          ELSE
              FACTOR = 1
          END IF
          X(J) = X(J) * FACTOR / RMS
      END DO
      CALL JTY_FFT2C(X,LOG2N,-1)
      CALL JTY_CRVECTOR(N,X,R)
      RETURN
      END
