      SUBROUTINE JTY_GAUSSIAN(N,SIGMA,X)
* Multiplies the ft X by a gaussian of rms SIGMA in real space
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that.
*                                HME/UoE, Starlink. 01-SEP-1992.
      COMPLEX*8 X(1)
      PARAMETER (PI=3.14159265)

      WIDTH = N / (2*PI*SIGMA)
      DO J = 1,N
          NUMBER = J - 1
          IF(J.GE.N/2+1) NUMBER = NUMBER - N
          ARG = -.5*(NUMBER/WIDTH)*(NUMBER/WIDTH)
          IF(ARG.LT.-40) THEN
              FACTOR = 0
          ELSE
              FACTOR = EXP(ARG)
          END IF
          X(J) = X(J) * FACTOR
      END DO
      RETURN
      END
