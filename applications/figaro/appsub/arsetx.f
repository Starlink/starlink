C+
      SUBROUTINE ARSETX(NX,COEFFS,ORDER,XVALS)
C
C     A R S E T X
C
C     Fills the x-value array for the arc with the wavelengths
C     given by the wavelength polynomial.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NX      (Integer) The number of x-values
C     (>) COEFFS  (Double precision array COEFFS(ORDER)) The
C                 wavelength coefficients.
C     (>) ORDER   (Integer) The number of wavelength coeffs.
C     (<) XVALS   (Real array XVALS(NX)) The array to be filled.
C
C     Functions / subroutines used -
C
C     GEN_EPOLYD  (GEN_ package)  Evaluate a polynomial
C
C                                         KS / CIT 18th Jan 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ORDER,NX
      REAL XVALS(NX)
      DOUBLE PRECISION COEFFS(ORDER)
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
         XVALS(I)=GEN_EPOLYD(DBLE(I),COEFFS,ORDER)
      END DO
C
      END
