C+
      DOUBLE PRECISION FUNCTION GEN_EPOLYD(VALUE,COEFFS,NCOEFF)
C
C     G E N _ E P O L Y D
C
C     Evaluates a polynomial - this is an interim version.
C     Ideally the VAX POLYD instruction should be used.
C
C     Parameters - (">" input, "<" output)
C
C     (>) VALUE   (Double precision) The value for which the
C                 polynomial is to be evaluated.
C     (>) COEFFS  (Double precision  array COEFFS(NCOEFF)) The
C                 coefficients of the polynomial, with the
C                 constant term last.
C     (>) NCOEFF  (Integer) The number of polynomial coefficients.
C
C     Returns -
C
C     (<) GEN_EPOLYD  (Double precision) The value of the polynomial.
C
C                                  KS / CIT 6th May 1983
C     History:
C
C     5th  Feb 1988  (CKL/CIT) VALUE changed from REAL to DOUBLE.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NCOEFF
      DOUBLE PRECISION VALUE,COEFFS(NCOEFF)
C
C     Local variables
C
      INTEGER NORD
C
C     Evaluate poly
C
      GEN_EPOLYD=0.
      DO NORD=1,NCOEFF
         GEN_EPOLYD=GEN_EPOLYD*VALUE+COEFFS(NORD)
      END DO
C
      END
