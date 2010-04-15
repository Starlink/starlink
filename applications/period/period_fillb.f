
C===========================================================================

      FUNCTION PERIOD_FILLB(MB, B, HWIDTH, PINCR)

C===========================================================================
C Fills the restoring beam B(0:MB) with a Gaussian of half-width HWIDTH
C (in Index units). The complex phase is zero for I=0 and increases
C by PINCR for each element. Returns the maximum filled element.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992.
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER MB,I,MFILL
      INTEGER PERIOD_FILLB
      DOUBLE PRECISION CONST,HWIDTH,PINCR
      DOUBLE PRECISION SIGMA,X,GAUSS,PHASEI
      DOUBLE COMPLEX B(0:MB)

C---------------------------------------------------------------------------
C Set up some initial conditions.
C---------------------------------------------------------------------------

      DO 100 I = 0, MB
         B(I) = (0.0D0, 0.0D0)
 100  CONTINUE

C---------------------------------------------------------------------------
C If Gaussian, fit a Gaussian beam.
C Calculate SIGMA and the normalization constant.
C---------------------------------------------------------------------------

      SIGMA = HWIDTH/DSQRT(2.0D0*DLOG(2.0D0))       ! Sigma in element units.
      CONST = 1.0D0/(2.0D0*SIGMA*SIGMA)             ! Normalization constant.
      MFILL = IDINT(5.0D0*SIGMA) + 1                ! Maximum filled element.
      IF ( MFILL.GT.MB ) MFILL = MB

C---------------------------------------------------------------------------
C Fill B with the Gaussian.
C---------------------------------------------------------------------------

      DO 200 I = 0, MFILL
         X = DFLOAT(I*I)
         GAUSS = DEXP(-CONST*X)
         B(I) = DCMPLX(GAUSS, 0.0D0)
 200  CONTINUE

C---------------------------------------------------------------------------
C Include the phase information.
C---------------------------------------------------------------------------

      DO 300 I = 1, MFILL
         PHASEI = DFLOAT(I)*PINCR
         B(I) = B(I)*DCMPLX(DCOS(PHASEI), DSIN(PHASEI))
 300  CONTINUE

C---------------------------------------------------------------------------
C Return the maximum filled element.
C---------------------------------------------------------------------------

      PERIOD_FILLB = MFILL

      RETURN
      END
