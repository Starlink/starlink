      REAL FUNCTION JTY_RMSFILTER(N,K1,K2,K3,K4,X)
* Return the rms of a real function passed through a bandpass filter in
* Fourier space. The bandpass is the square root of a cosine bell.
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that.
*                                HME/UoE, Starlink. 01-SEP-1992.
      COMPLEX*8 X(1)
      PARAMETER ( PI=3.14159 )
      JTY_RMSFILTER = 0
      DO K = K1,K4
          IF(K.EQ.0.OR.K.EQ.N/2) THEN
              F = 1
          ELSE
              F = 2
          END IF
          IF(K.LT.K2) THEN
              ARG = PI * FLOAT(K-K1)/FLOAT(K2-K1)
              FACTOR = .5 * (1-COS(ARG))
          ELSE IF(K.GT.K3) THEN
              ARG = PI * FLOAT(K-K3)/FLOAT(K4-K3)
              FACTOR = .5 * (1+COS(ARG))
          ELSE
              FACTOR = 1
          END IF
          JTY_RMSFILTER = JTY_RMSFILTER + F * FACTOR *
     1    (REAL(X(K+1))*REAL(X(K+1)) + AIMAG(X(K+1))*AIMAG(X(K+1)))
      END DO
      JTY_RMSFILTER = SQRT(JTY_RMSFILTER) / N   !Because it is the transform
      RETURN
      END
