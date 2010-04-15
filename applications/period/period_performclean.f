
      SUBROUTINE PERIOD_PERFORMCLEAN(XDATA, YDATA, NDATA, MAXPTS,
     :                               FMIN, FMAX, FINT, NCL, GAIN,
     :                               FREQUENCY, POWER, NOUT, INFO,
     :                               ONES, WFREQ, DFREQ, D, W, R,
     :                               B, C, S)

C===========================================================================
C Calculates the CLEANed power spectrum of a dataset of length NDATA stored
C in the arrays XDATA and YDATA. The spectrum will be calculated between the
C frequencies FMIN and FMAX, with a step size given by FINT. The number
C of CLEAN iterations is set by NCL and the loop gain is set by GAIN.
C PERIOD_CLEAN returns POWER (the CLEANed power), FREQUENCY (the corresponding
C frequencies), and NOUT (the number of points in POWER and FREQUENCY, which
C cannot be greater than MAXPTS). If INFO=1 then loop information is output
C to the screen.
C
C BRIEF DESCRIPTION OF USE:
C
C PERIOD_CLEAN deconvolves the spectral window from the dirty spectrum by
C using an iterative, one-dimensional, Hoegbom CLEAN algorithm. To produce
C the clean spectrum, a Gaussian beam is used which has been fitted to the
C HWHM of the primary peak in the spectral window; the phases are determined
C by the offset of the mean time TMEAN from the "origin" TZERO. Since all
C spectra from real data are Hermitian, we define only the non-negative
C frequencies.  The negative frequency elements are recovered by the use of
C the function PERIOD_CVAL, which returns the complex conjugate for negative
C frequencies, and zero for frequencies outside the defined range.
C
C Essentially adapted for PERIOD by Vikram Singh Dhillon
C                                         @Sussex 24-June-1992, based on
C the original code written by Joseph Lehar (JLEHAR@MAIL.AST.CAM.AC.UK).
C
C GJP March 1997
C
C Modified ONES array dimensions since it was 0-MAXPNTS-1 and
C 1-MAXPNTS were actually being used. Modified calls to
C PERIOD_DFOUR to use FREQ rather than array (say) DFREQ(I) value.
C
C Converted to Double Precision (KPD), August 2001
C Power-raising modified to use INTEGER power (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE "PIVARS"

C-----------------------------------------------------------------------------
C PERIOD_CLEAN declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, MAXPTS, NCL, NOUT, I
      INTEGER NDFREQ, NWFREQ, NBFREQ
      INTEGER ICL, L, PERIOD_MAXLOC
      INTEGER ISTEP, LOOP, INFO
      INTEGER PERIOD_FILLB
      DOUBLE PRECISION XDATA(NDATA),YDATA(NDATA),ONES(MAXPTS)
      DOUBLE PRECISION POWER(MAXPTS),FREQUENCY(MAXPTS)
      DOUBLE PRECISION WFREQ(0:(2*MAXPTS)-1), DFREQ(0:MAXPTS-1)
      DOUBLE PRECISION FMIN, FMAX, FINT, FREQ
      DOUBLE PRECISION GAIN
      DOUBLE PRECISION PHINC, PERIOD_HWHM, HWIDTH
      DOUBLE PRECISION TZERO, TMEAN, DMEAN, ADEV, SDEV, VAR
      DOUBLE PRECISION TOLFACT
      DOUBLE COMPLEX PERIOD_DFOUR, CC, PERIOD_ALPHA
      DOUBLE COMPLEX D(0:MAXPTS-1), W(0:(2*MAXPTS)-1), R(0:MAXPTS-1)
      DOUBLE COMPLEX B(0:MAXPTS-1), C(0:MAXPTS-1),     S(0:MAXPTS-1)
      DATA ISTEP/50/


C----------------------------------------------------------------------------
C Initialise the C array.
C----------------------------------------------------------------------------

      DO 10 I = 0, MAXPTS - 1
         C(I) = (0.0D0,0.0D0)
  10  CONTINUE

C----------------------------------------------------------------------------
C Find the mean time and data mean and subtract (for simplicity). Also fill
C the ONES array with 1's in order to calculate the spectral window. The
C time information TZERO and TMEAN are used later in the CLEANing for beam
C phases.
C-----------------------------------------------------------------------------

      CALL PERIOD_MOMENT(XDATA, NDATA, TMEAN, ADEV, SDEV, VAR)
      CALL PERIOD_MOMENT(YDATA, NDATA, DMEAN, ADEV, SDEV, VAR)

      TZERO = TMEAN
      DO 20 I = 1, NDATA
         XDATA(I) = XDATA(I) - TMEAN
         YDATA(I) = YDATA(I) - DMEAN
         ONES(I)  = 1.0D0
  20  CONTINUE

*    This line included because of a fault in the original code.
      ONES(1)=0.0D0

C-----------------------------------------------------------------------------
C Calculate the dirty spectrum D (using a normal discrete Fourier transform).
C Fill the residual array R with the dirty spectrum.
C-----------------------------------------------------------------------------

      IF ( INFO.EQ.1 ) THEN
         WRITE (*, *) '** OK: Calculating window and dirty spectra...'
         WRITE (*, *) ' '
         WRITE (*, *) ' '
      END IF

      LOOP = 0
      I = 0

C A tolerance factor has been introduced to ensure that FREQ is allowed
C  to reach FMAX when rounding errors would otherwise cause it to fall
C  short by FINT. This is a consequence of using non-integer Do Loop
C  variables! (KPD)
      TOLFACT = FINT*1.0D-6

      FREQ = FMIN
      DO 30 WHILE( FREQ .LE. FMAX+TOLFACT )
         WFREQ(I) = DCMPLX(FREQ,0.0D0)
         DFREQ(I) = DCMPLX(FREQ,0.0D0)

         W(I) = PERIOD_DFOUR(NDATA, XDATA, ONES, FREQ)   ! Spectral window.
         D(I) = PERIOD_DFOUR(NDATA, XDATA, YDATA, FREQ)  ! Dirty spectrum.

         R(I) = D(I)
         IF ( INFO.EQ.1 ) THEN
            IF ( I.EQ.(LOOP*ISTEP) ) THEN
               WRITE (*, 99001) DFREQ(I),(CDABS(D(I)))**2
99001          FORMAT ('+Frequency =', D12.6, ',  Dirty Power =', D18.6)
               LOOP = LOOP + 1
            END IF
         END IF
         I = I + 1
         FREQ = FREQ + FINT
  30  CONTINUE
      NDFREQ = I - 1

C-----------------------------------------------------------------------------
C Complete the spectral window.
C-----------------------------------------------------------------------------

      IF ( INFO.EQ.1 ) THEN
         WRITE (*, *) ' '
         WRITE (*, *) '** OK: Completing spectral window...'
         WRITE (*, *) ' '
         WRITE (*, *) ' '
      END IF

      FREQ = FMAX + FINT
      DO 40 WHILE ( FREQ .LE. (2.0D0*FMAX)-FMIN+TOLFACT )
         WFREQ(I) = DCMPLX(FREQ,0.0D0)

         W(I) = PERIOD_DFOUR(NDATA, XDATA, ONES, FREQ)

         IF ( INFO.EQ.1 ) THEN
            IF ( I.EQ.(LOOP*ISTEP) ) THEN
               WRITE (*, 99002) WFREQ(I),(CDABS(W(I)))**2
99002          FORMAT ('+Frequency =', D12.6, ',  Window Power =',
     :                 D18.6)
               LOOP = LOOP + 1
            END IF
         END IF
         I = I + 1
         FREQ = FREQ + FINT
  40  CONTINUE
      NWFREQ = I - 1

C   Ensure that NDFREQ = NWFREQ/2
      IF ( (2*NDFREQ).GT.NWFREQ ) NDFREQ = NWFREQ/2

      WRITE (*, *) 'I = ', I
      WRITE (*, *) 'LOOP = ', LOOP
      WRITE (*, *) 'MAXPTS = ', MAXPTS

C-----------------------------------------------------------------------------
C Fit a restoring beam B to the spectral window peak.
C-----------------------------------------------------------------------------

      PHINC = TWOPI*FINT*(TZERO-TMEAN)          ! Phase increment per element.
      HWIDTH = PERIOD_HWHM(NWFREQ, W)

      IF ( HWIDTH.LE.0.0D0 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: Could not find half-width and'
         WRITE (*, *) '** ERROR: half-maximum in PERIOD_HWHM.'
         WRITE (*, *) 'HWIDTH = ', HWIDTH
         NOUT = 0
         GO TO 700
      END IF

      NBFREQ = PERIOD_FILLB(MAXPTS-1, B, HWIDTH, PHINC)! Fill the restoring beam

C-----------------------------------------------------------------------------
C CLEAN the residual spectrum, storing the components in C(0:MS).
C-----------------------------------------------------------------------------

      IF ( INFO.EQ.1 ) THEN
         WRITE (*, *) ' '
         WRITE (*, *) '** OK: Performing CLEAN iterations...'
         WRITE (*, *) ' '
         WRITE (*, *) ' '
      END IF

      DO 50 ICL = 1, NCL
         L = PERIOD_MAXLOC(NDFREQ, R)      ! Element location of max peak in R
         CC = GAIN*PERIOD_ALPHA(L, NDFREQ, R, W)! Estimate component to remove

         CALL PERIOD_SUBCMP(NDFREQ, R, W, L, CC)  ! Subtract component from R.

         C(L) = C(L) + CC                         ! Store component in C.

         IF ( INFO.EQ.1 ) THEN
            WRITE (*, 99003) ICL
99003       FORMAT ('+Clean iteration = ', I4)
         END IF

  50  CONTINUE

C-----------------------------------------------------------------------------
C Generate the clean spectrum S(0:MS).
C-----------------------------------------------------------------------------

      IF ( INFO.EQ.1 ) THEN
         WRITE (*, *) ' '
         WRITE (*, *) '** OK: Generating CLEANed spectrum...'
         WRITE (*, *) ' '
         WRITE (*, *) ' '
      END IF
      CALL PERIOD_CONVOLV(NDFREQ, S, NDFREQ, C, NBFREQ, B,  ! Convolve C with
     :                    DFREQ, INFO)                      ! B to form S.

      CALL PERIOD_ADD(NDFREQ, S, NDFREQ, S, NDFREQ, R)      ! Then add R to S.

C-----------------------------------------------------------------------------
C Finally, load the output arrays with the POWER of the CLEANed spectrum and
C their corresponding FREQUENCIES.
C-----------------------------------------------------------------------------

      DO 60 I = 0, NDFREQ
         POWER(I+1) = CDABS(S(I))*CDABS(S(I))
         FREQUENCY(I+1) = DFREQ(I)
  60  CONTINUE
      NOUT = NDFREQ + 1

 700  CONTINUE
      RETURN
      END
