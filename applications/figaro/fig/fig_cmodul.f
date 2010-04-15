C+
      SUBROUTINE FIG_CMODUL (NELM,RDATA,IDATA,MDATA)
C
C     F I G _ C M O D U L
C
C     Given a complex array, calculates its modulus - ie returns a
C     real array in which each element is the modulus of the corresponding
C     complex element.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NELM    (Integer) The number of elements in the arrays.
C     (>) RDATA   (Double precision array RDATA(NELM)) The real part
C                 of the complex FFT.
C     (>) IDATA   (Double precision array IDATA(NELM)) The imaginary
C                 part of the complex FFT.
C     (<) MDATA   (Real array MDATA(NELM)) The resulting modulus array.
C
C                 Note that the arrays may in fact be multi-dimensional -
C                 they are treated here as 1D for generality.
C
C     Common variables used -  None
C
C                                          KS / AAO.  1st Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL MDATA(NELM)
      DOUBLE PRECISION RDATA(NELM), IDATA(NELM)
C
C     Local variables
C
      INTEGER I
C
C     Calculate power spectrum
C
      DO I=1,NELM
         MDATA(I)=SQRT(RDATA(I)*RDATA(I)+IDATA(I)*IDATA(I))
      END DO
C
      END
