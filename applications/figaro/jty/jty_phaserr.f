      SUBROUTINE JTY_PHASERR(N,SHIFT,X,AV,K1,K2,DEV)
* Computes the phase of a complex function on the assumption that it is
* within +/- pi of 2*pi*SHIFT*j/N. It then computes K1 and K2 beyond which
* the phase is random, and DEV of the phase within K1 and K2 (1 = random)
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that.
*                                HME/UoE, Starlink. 01-SEP-1992.
      COMPLEX*8 X(1)
      REAL*4 AV(1)
      PARAMETER ( PI = 3.14159265 )
      PARAMETER (NAV=50)

      DO I = 1,N/2
          AV(I) = 0
      END DO
      DO I = 1,N/2
          ANGLE = 2*PI*(I-1)*SHIFT/N
          IF(X(I).EQ.0) THEN
              PHASE = ANGLE
          ELSE
              PHASE = ATAN2(AIMAG(X(I)),REAL(X(I)))
          END IF
          NPI = (PHASE-ANGLE) / PI
          IF(NPI.GT.0) THEN
              NCYCLE = (NPI + 1) / 2
          ELSE
              NCYCLE = (NPI - 1) / 2
          END IF
          PHASE = PHASE - 2*PI*NCYCLE - ANGLE
          DO J = -NAV,NAV
              IF(I+2*J.GE.1.AND.I+2*J.LE.N/2) THEN
                  NS = MIN(NAV,MIN(I+J-1,N/2-(I+J)))
                  IF(NS.EQ.0) THEN
                      FRAC = 1
                  ELSE
                      FRAC = (1 - ABS(FLOAT(J))/NS) / NS
                  END IF
                  AV(I+J) = AV(I+J) + 2 / PI * ABS(PHASE) * FRAC
              END IF
          END DO
      END DO
      DEV = 2
      DO I = 5,N/2-NAV
          DEV = AMIN1(DEV,AV(I))
      END DO
      K1 = -1
      K2 = -1
      CUT = 0.5 * (1 + DEV)
      DO I = 5,N/2-NAV
          IF(K1.EQ.-1.AND.AV(I).LT.CUT) K1 = I - 1
          IF(K1.NE.-1.AND.K2.EQ.-1.AND.AV(I).GT.CUT) K2 = I-2
      END DO
      RETURN
      END
