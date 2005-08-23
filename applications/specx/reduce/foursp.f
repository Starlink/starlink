*  History:
*     22 Nov 1993 (hme):
*        Remove TABs.
C-----------------------------------------------------------------------

      SUBROUTINE FOURSP (X, BUF, NPTS, BAD)

C  Routine for direct Fourier transform of real symmetric data ( e.g.
C  power spectra and A.C.F.s

      IMPLICIT  NONE

C     Formal parameters

      REAL      X(*)
      REAL      BUF(*)
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
      REAL      ROOTN

      REAL      PI
      DATA      PI /3.141592654/

C  Ok, go...

      IMOD = 1
      NT   = (NPTS-1)
      ANT  = NT

      DO 10 I = 1, NPTS
        IF (X(I).NE.BAD) THEN
          BUF(I) = X(I)
        ELSE
          BUF(I) = 0.0
        END IF
   10 CONTINUE

      DO 100 M = 1, NPTS
        MM     = M-1
        X(M)   = BUF(1) + FLOAT(IMOD)*BUF(NPTS)

        AMM    = MM
        COSY   = COS(PI*AMM/ANT)
        SINY   = SIN(PI*AMM/ANT)
        COSKY  = COSY
        SINKY  = SINY

        DO 50 K = 1 , NPTS-2
          X(M)   = X(M) + 2.*BUF(K+1)*COSKY
          COSKY1 = COSKY*COSY - SINKY*SINY
          SINKY1 = COSKY*SINY + SINKY*COSY
          COSKY  = COSKY1
          SINKY  = SINKY1
   50 CONTINUE

        IMOD = -IMOD
  100 CONTINUE

      ROOTN = SQRT (FLOAT (2*NT))

      DO 150 I = 1, NPTS
        X(I) = X(I)/ROOTN
  150 CONTINUE

      RETURN
      END

