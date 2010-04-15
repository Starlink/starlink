
      SUBROUTINE PERIOD_CLEAN(XDATA, YDATA, NDATA, MAXPTS, FMIN, FMAX,
     :                        FINT, NCL, GAIN, FREQUENCY, POWER, NOUT,
     :                        INFO)

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
C Written by Kevin P Duffey @RAL, October 2001, to make use of routine
C  PERIOD_PERFORMCLEAN (formerly PERIOD_CLEAN, adapted for PERIOD by
C                        Vikram Singh Dhillon @Sussex 24-June-1992, based
C                        on the original code written by Joseph Lehar
C                        (JLEHAR@MAIL.AST.CAM.AC.UK).)
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

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PERIOD_CLEAN declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, MAXPTS, NCL, NOUT, INFO
      DOUBLE PRECISION XDATA(NDATA),YDATA(NDATA)
      DOUBLE PRECISION POWER(MAXPTS),FREQUENCY(MAXPTS)
      DOUBLE PRECISION GAIN
      INTEGER ONESPTR, WFREQPTR, DFREQPTR
      INTEGER DPTR, WPTR, RPTR
      INTEGER BPTR, CPTR, SPTR
      DOUBLE PRECISION FMIN, FMAX, FINT


      CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, ONESPTR)
      CALL PERIOD_ALLOC('_DOUBLE', 2*MAXPTS, WFREQPTR)
      CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, DFREQPTR)
C  NOTE that the next six allocations are for DOUBLE COMPLEX arrays;
C    KPD's fudge is to request an equivalent allocation sufficient
C    for twice the number of DOUBLE COMPLEX elements, but expressed
C    as DOUBLEs; this is because PSX_CALLOC, which gets invoked
C    "down the line" doesn't allow for a COMPLEX attribute.
      CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*2, DPTR)
      CALL PERIOD_ALLOC('_DOUBLE', 2*MAXPTS*2, WPTR)
      CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*2, RPTR)
      CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*2, BPTR)
      CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*2, CPTR)
      CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*2, SPTR)

C----------------------------------------------------------------------------
C Calculate CLEANed power spectrum
C----------------------------------------------------------------------------

      CALL PERIOD_PERFORMCLEAN(XDATA, YDATA, NDATA, MAXPTS, FMIN,
     :                         FMAX, FINT, NCL, GAIN, FREQUENCY,
     :                         POWER, NOUT, INFO,
     :                         %VAL(CNF_PVAL(ONESPTR)),
     :                         %VAL(CNF_PVAL(WFREQPTR)),
     :                         %VAL(CNF_PVAL(DFREQPTR)),
     :                         %VAL(CNF_PVAL(DPTR)),
     :                         %VAL(CNF_PVAL(WPTR)),
     :                         %VAL(CNF_PVAL(RPTR)),
     :                         %VAL(CNF_PVAL(BPTR)),
     :                         %VAL(CNF_PVAL(CPTR)),
     :                         %VAL(CNF_PVAL(SPTR)))

      CALL PERIOD_DEALL(SPTR)
      CALL PERIOD_DEALL(CPTR)
      CALL PERIOD_DEALL(BPTR)
      CALL PERIOD_DEALL(RPTR)
      CALL PERIOD_DEALL(WPTR)
      CALL PERIOD_DEALL(DPTR)
      CALL PERIOD_DEALL(DFREQPTR)
      CALL PERIOD_DEALL(WFREQPTR)
      CALL PERIOD_DEALL(ONESPTR)

      RETURN
      END
