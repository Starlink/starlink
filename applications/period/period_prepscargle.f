
      SUBROUTINE PERIOD_PREPSCARGLE(X, Y, N, FMAX, FINT, NDIM,
     :                              XMIN, XDIF, OFAC, HIFAC, NFREQ)

C==============================================================================
C Sizes the Work array for use in SCARGLE.
C
C Written by Kevin P Duffey @RAL, November 2001
C
C Converted to Double Precision (KPD), August 2001
C==============================================================================

      IMPLICIT NONE

C------------------------------------------------------------------------------
C Number of interpolation points per 1/4 cycle of highest frequency.
C------------------------------------------------------------------------------

      INTEGER MACC
      PARAMETER (MACC=4)

C------------------------------------------------------------------------------
C PERIOD_SCARGLE declarations.
C------------------------------------------------------------------------------

      INTEGER     N, K
      INTEGER     NDIM, J, NFREQ, NFREQT
      DOUBLE PRECISION FMAX, FINT
      DOUBLE PRECISION X(N),Y(N)
      DOUBLE PRECISION XMIN, XMAX, XDIF
      DOUBLE PRECISION OFAC, HIFAC

C------------------------------------------------------------------------------
C Compute the range of the data.
C------------------------------------------------------------------------------

      XMIN = X(1)
      XMAX = XMIN
      DO 100 J = 2, N
         IF ( X(J).LT.XMIN ) XMIN = X(J)
         IF ( X(J).GT.XMAX ) XMAX = X(J)
 100  CONTINUE
      XDIF = XMAX - XMIN

C------------------------------------------------------------------------------
C Compute OFAC and HIFAC.
C------------------------------------------------------------------------------

      OFAC = 1.0D0/(FINT*XDIF)
      HIFAC = (FMAX/FINT)/(0.5D0*OFAC*DFLOAT(N))

C------------------------------------------------------------------------------
C Size the FFT as next power of 2 above NFREQT.
C------------------------------------------------------------------------------

      NFREQT = IDINT(OFAC*HIFAC*DFLOAT(N)*MACC)

      NFREQ = 64
 200  CONTINUE
      IF ( NFREQ.LT.NFREQT ) THEN
         NFREQ = NFREQ*2
         GO TO 200
      END IF
      NDIM = 2*NFREQ

      RETURN
      END
