      SUBROUTINE JTY_POLAR(N,FT,R,T)
      REAL*4 R(1), T(1)
      COMPLEX*8 FT(1)
      DO J = 1,N
          R(J) = CABS(FT(J))
          IF(R(J).GT.0) THEN
              T(J) = ATAN2(AIMAG(FT(J)),REAL(FT(J)))
          ELSE
              T(J) = 0
          END IF
      END DO
      RETURN
      END
