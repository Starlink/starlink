      SUBROUTINE JTY_PHASEBAND(N,SHIFT,X,PHASE)
* Computes the phase of a complex function on the assumption that it is
* within +/- pi of 2*pi*SHIFT*j/N
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that.
*                                HME/UoE, Starlink. 01-SEP-1992.
      REAL*4 PHASE(1)
      COMPLEX*8 X(1)
      PARAMETER ( PI = 3.14159265 )
      DO I = 1,N/2
          ANGLE = 2*PI*(I-1)*SHIFT/N
          IF(X(I).EQ.0) THEN
              PHASE(I) = ANGLE
          ELSE
              PHASE(I) = ATAN2(AIMAG(X(I)),REAL(X(I)))
          END IF
          NPI = (PHASE(I)-ANGLE) / PI
          IF(NPI.GT.0) THEN
              NCYCLE = (NPI + 1) / 2
          ELSE
              NCYCLE = (NPI - 1) / 2
          END IF
          PHASE(I) = PHASE(I) - 2*PI*NCYCLE
          IF(I.LT.N/2) PHASE(I+N/2) = PHASE(I) - ANGLE
      END DO
      RETURN
      END
