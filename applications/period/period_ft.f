
      SUBROUTINE PERIOD_FT(XDATA, YDATA, NDATA, MAXPTS, FMIN, FMAX,
     :                     FINT, FREQUENCY, POWER, NOUT, INFO)

C===========================================================================
C Calculates the Fourier transform of a dataset of length NDATA stored in
C the arrays XDATA and YDATA. The FT will be calculated between the
C frequencies FMIN and FMAX, with a step size given by FINT. PERIOD_FT returns
C POWER (the amplitude squared divided by NDATA**2 for normalisation),
C FREQUENCY (the corresponding frequencies) and NOUT (the number of points
C in POWER and FREQUENCY, which cannot be greater than MAXPTS). If INFO=1,
C then loop information is output to the screen.
C
C Written by Kevin P Duffey @RAL, October 2001, to make use of routine
C  PERIOD_PERFORMFT (formerly PERIOD_FT, written by Vikram Singh Dhillon
C                     @Sussex 24-June-1992.
C
C Converted to Double Precision (KPD), August 2001
C Power-raising modified to use INTEGER power (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PERIOD_FT declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, MAXPTS, NOUT, INFO
      INTEGER FT_REALPTR, FT_IMAGPTR
      DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA)
      DOUBLE PRECISION POWER(MAXPTS), FREQUENCY(MAXPTS)
      DOUBLE PRECISION FMIN, FMAX, FINT


      CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, FT_REALPTR)
      CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, FT_IMAGPTR)

C-----------------------------------------------------------------------------
C Perform Fourier transform; accumulate power spectrum; fill frequency array.
C-----------------------------------------------------------------------------

      CALL PERIOD_PERFORMFT(XDATA, YDATA, NDATA, MAXPTS, FMIN, FMAX,
     :                      FINT, FREQUENCY, POWER, NOUT, INFO,
     :                      %VAL(CNF_PVAL(FT_REALPTR)),
     :                      %VAL(CNF_PVAL(FT_IMAGPTR)))

      CALL PERIOD_DEALL(FT_IMAGPTR)
      CALL PERIOD_DEALL(FT_REALPTR)

      RETURN
      END
