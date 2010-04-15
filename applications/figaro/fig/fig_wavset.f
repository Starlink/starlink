C+
      SUBROUTINE FIG_WAVSET(NX,COEFFS,NCOEFF,XDATA)
C
C     F I G _ W A V S E T
C
C     Sets a wavelength array according to a set of polynomial
C     coefficients.  Note: coeffs should assume that channel N
C     goes from N-1/2 to N+1/2, ie N.0 is the channel CENTER.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NX       (Integer) Number of elements in the wavelength
C                  array.
C     (>) COEFFS   (Real array COEFFS(NCOEFF)) Polynomial coeff-
C                  icients, with constant term last.
C     (>) NCOEFF   (Integer) Number of coefficients.
C     (<) XDATA    (Real array XDATA(NX)) Wavelength array.
C
C     Common variables used -  None
C
C     Functions / subroutines used -
C
C     GEN_EPOLY   (GEN_ package) Evaluate a polynomial.
C
C                                             KS / AAO 9th Nov 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NCOEFF
      REAL    COEFFS(NCOEFF), XDATA(NX)
C
C     Functions
C
      REAL GEN_EPOLY
C
C     Local variables
C
      INTEGER IX
C
      DO IX=1,NX
         XDATA(IX)=GEN_EPOLY(FLOAT(IX),COEFFS,NCOEFF)
      END DO
C
      END
