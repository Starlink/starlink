C+
      SUBROUTINE ARC_ARSETXD(NX,COEFFS,NCOEFF,XDVALS)
C
C     A R S E T X D
C
C     Fills the double precision x-value array for the arc with
C     the wavelengths given by the wavelength polynomial.  This is
C     just a double precision version of ARSETX.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NX      (Integer) The number of x-values
C     (>) COEFFS  (Double precision array COEFFS(NCOEFF)) The
C                 wavelength coefficients.
C     (>) NCOEFF  (Integer) The number of wavelength coeffs.
C     (<) XDVALS  (Double precision array XDVALS(NX)) The array
C                 to be filled.
C
C     Functions / subroutines used -
C
C     GEN_EPOLYD  (GEN_ package)  Evaluate a polynomial
C
C                                         KS / AAO 12th Sept 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NCOEFF,NX
      DOUBLE PRECISION COEFFS(NCOEFF), XDVALS(NX)
C
C     Functions
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NX
         XDVALS(I)=GEN_EPOLYD(DBLE(I),COEFFS,NCOEFF)
      END DO
C
      RETURN
      END
