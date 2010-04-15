
      SUBROUTINE PERIOD_PERFORMFT(XDATA, YDATA, NDATA, MAXPTS,
     :                            FMIN, FMAX, FINT, FREQUENCY,
     :                            POWER, NOUT, INFO,
     :                            FT_REAL, FT_IMAG)

C===========================================================================
C Calculates the Fourier transform of a dataset of length NDATA stored in
C the arrays XDATA and YDATA. The FT will be calculated between the
C frequencies FMIN and FMAX, with a step size given by FINT. PERIOD_FT returns
C POWER (the amplitude squared divided by NDATA**2 for normalisation),
C FREQUENCY (the corresponding frequencies) and NOUT (the number of points
C in POWER and FREQUENCY, which cannot be greater than MAXPTS). If INFO=1,
C then loop information is output to the screen.
C
C Essentially written by Vikram Singh Dhillon @Sussex 24-June-1992,
C  as PERIOD_FT.
C
C Converted to Double Precision (KPD), August 2001
C Power-raising modified to use INTEGER power (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE "PIVARS"

C-----------------------------------------------------------------------------
C PERIOD_FT declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, MAXPTS, NOUT
      INTEGER COUNTER, LOOP, ISTEP, I, INFO
      DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA)
      DOUBLE PRECISION POWER(MAXPTS), FREQUENCY(MAXPTS)
      DOUBLE PRECISION FT_REAL(MAXPTS), FT_IMAG(MAXPTS)
      DOUBLE PRECISION FMIN, FMAX, FINT
      DOUBLE PRECISION OMEGA, FREQ
      DOUBLE PRECISION EXPO, C, S
      DOUBLE PRECISION TOLFACT
      DATA ISTEP/50/


C-----------------------------------------------------------------------------
C Initialise counters.
C-----------------------------------------------------------------------------

      LOOP = 0
      COUNTER = 1

C-----------------------------------------------------------------------------
C Loop over frequency range.
C-----------------------------------------------------------------------------

C A tolerance factor has been introduced to ensure that FREQ is allowed
C  to reach FMAX when rounding errors would otherwise cause it to fall
C  short by FINT. This is a consequence of using non-integer Do Loop
C  variables! (KPD)
      TOLFACT = FINT*1.0D-4

      FREQ = FMIN
      DO 20 WHILE ( FREQ .LE. ( FMAX+TOLFACT ) )

C-----------------------------------------------------------------------------
C Initialise sums for each new frequency. The real part of the Fourier
C transform is stored in FT_REAL and the imaginary part in FT_IMAG.
C-----------------------------------------------------------------------------

         FT_REAL(COUNTER) = 0.0D0
         FT_IMAG(COUNTER) = 0.0D0

C-----------------------------------------------------------------------------
C Calculate the angular frequency OMEGA.
C-----------------------------------------------------------------------------

         OMEGA = TWOPI*FREQ

C-----------------------------------------------------------------------------
C Loop over number of data points.
C-----------------------------------------------------------------------------

         DO 10 I = 1, NDATA

C-----------------------------------------------------------------------------
C Calculate the Fourier transform at frequency FREQ.
C-----------------------------------------------------------------------------

            EXPO = OMEGA*XDATA(I)
            C = DCOS(EXPO)
            S = DSIN(EXPO)
            FT_REAL(COUNTER) = FT_REAL(COUNTER) + (YDATA(I)*C)
            FT_IMAG(COUNTER) = FT_IMAG(COUNTER) + (YDATA(I)*S)
  10     CONTINUE

C-----------------------------------------------------------------------------
C Accumulate the power spectrum by summing the amplitude squared of the
C Fourier transform over the full frequency range. Divide by the number of
C data points squared in order to normalise the spectral window to unit
C amplitude at zero frequency.
C-----------------------------------------------------------------------------

         POWER(COUNTER) = ((FT_REAL(COUNTER)**2)+(FT_IMAG(COUNTER)**2)
     :                    )/DFLOAT(NDATA**2)

C-----------------------------------------------------------------------------
C Fill the frequency array, ouput some loop information and return.
C-----------------------------------------------------------------------------

         FREQUENCY(COUNTER) = FREQ

         IF ( INFO.EQ.1 ) THEN
            IF ( COUNTER-1.EQ.(LOOP*ISTEP) ) THEN
               WRITE (*, 99001) FREQUENCY(COUNTER), POWER(COUNTER)
99001          FORMAT ('+Frequency =', D12.6, ',  Normalised Power =',
     :                 D18.6)
               LOOP = LOOP + 1
            END IF
         END IF

         COUNTER = COUNTER + 1

         FREQ = FREQ + FINT

  20  CONTINUE
      NOUT = COUNTER - 1

      RETURN
      END
