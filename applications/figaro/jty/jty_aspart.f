      SUBROUTINE JTY_ASPART(N,K1,K2,K3,K4,SHIFT,X,ARMS,SRMS)
* Compute the anti and symmetric parts of a complex buffer, shifted by SHIFT
* Assume a transform of a real function, and a cosine bell of K1,K2,K3,K4.
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that.
*                                HME/UoE, Starlink. 01-SEP-1992.
      COMPLEX*8 X(N), PHASE
      PARAMETER ( PI = 3.14159265 )

      ARMS = 0
      SRMS = 0
      DO K = K1,K4
          ANGLE = -2*PI*K*SHIFT/N
          PHASE = CMPLX(COS(ANGLE),SIN(ANGLE))
          IF(K.EQ.0.OR.K.EQ.N/2) THEN
              F = 1
          ELSE
              F = 2
          END IF
          IF(K.LT.K2) THEN
              ARG = PI * FLOAT(K-K1)/FLOAT(K2-K1)
              F = F * .25 * (1-COS(ARG)) * (1-COS(ARG))
          ELSE IF(K.GT.K3) THEN
              ARG = PI * FLOAT(K-K3)/FLOAT(K4-K3)
              F = F * .25 * (1+COS(ARG)) * (1+COS(ARG))
          END IF
          ARMS = ARMS + F*AIMAG(PHASE*X(K+1))*AIMAG(PHASE*X(K+1))
          SRMS = SRMS + F*REAL(PHASE*X(K+1))*REAL(PHASE*X(K+1))
      END DO
      ARMS = SQRT(ARMS) / N ! Since it is the transform
      SRMS = SQRT(SRMS) / N
      RETURN
      END
