C-----------------------------------------------------------------------

      SUBROUTINE FOURIER_POWER_SPECTRUM (X, BUF, NPTS, BAD)

C  Routine for finding fourier power-spectrum of real data sequence
C  Parameters: X(NPTS).......On input:  Data to be transformed
C                            On output: Power spectrum
C              BUF(2*NPTS)...Workspace
C              NPTS..........Array dimension

      IMPLICIT  NONE

C     Formal parameters

      REAL      X(1)
      REAL      BUF(1)
      INTEGER   NPTS
      REAL      BAD

C     Local variables

      INTEGER   I
      INTEGER   K
      INTEGER   M
      INTEGER   MM
      INTEGER   IMOD
      INTEGER   NT
      REAL      AMM
      REAL      ANT
      REAL      COSY,   SINY
      REAL      COSKY,  SINKY
      REAL      COSKY1, SINKY1

      REAL      PI
      DATA      PI /3.141592654/
C  Ok, go...

      IMOD = 1
      NT   = (NPTS-1)
      ANT  = NT

      DO M = 1, NPTS
        MM= M - 1

        BUF(M)      = 0.
        BUF(NPTS+M) = 0.

        IF (X(1).NE.BAD) BUF(M) = 0.5*X(1)
        IF (X(NPTS).NE.BAD) BUF(M) = BUF(M) + 0.5*FLOAT(IMOD)*X(NPTS)

        AMM   = MM
        COSY  = COS(PI*AMM/ANT)
        SINY  = SIN(PI*AMM/ANT)
        COSKY = COSY
        SINKY = SINY

        DO K = 1, NPTS-2
          IF (X(K+1).NE.BAD) THEN
            BUF(M)      = BUF(M)+X(K+1)*COSKY
            BUF(NPTS+M) = BUF(NPTS+M)+X(K+1)*SINKY
          END IF
          COSKY1 = COSKY*COSY-SINKY*SINY
          SINKY1 = COSKY*SINY+SINKY*COSY
          COSKY  = COSKY1
          SINKY  = SINKY1
        END DO
        IMOD = -IMOD

      END DO

C  Normalize and compute squared modulus of complex array

      DO I = 1, NPTS
        X(I) = LOG10(4.*(BUF(I)**2+BUF(NPTS+I)**2)/ANT)
      END DO

      RETURN
      END

