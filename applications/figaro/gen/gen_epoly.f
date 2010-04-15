C+
      REAL FUNCTION GEN_EPOLY(VALUE,COEFFS,NCOEFF)
C
C     G E N _ E P O L Y
C
C     Evaluates a polynomial - this is an interim version.
C     Ideally the VAX POLY instruction should be used.
C
C     Parameters - (">" input, "<" output)
C
C     (>) VALUE   (Real) The value for which the polynomial is to
C                 be evaluated.
C     (>) COEFFS  (Real array COEFFS(NCOEFF) The
C                 coefficients of the polynomial, with the
C                 constant term last.
C     (>) NCOEFF  (Integer) The number of polynomial coefficients.
C
C     Returns -
C
C     (<) GEN_EPOLY   (Real) The value of the polynomial.
C
C                                  KS / CIT 6th May 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NCOEFF
      REAL VALUE
      REAL COEFFS(NCOEFF)
C
C     Local variables
C
      INTEGER NORD
C
C     Evaluate poly
C
      GEN_EPOLY=0.
      DO NORD=1,NCOEFF
         GEN_EPOLY=GEN_EPOLY*VALUE+COEFFS(NORD)
      END DO
C
      END
