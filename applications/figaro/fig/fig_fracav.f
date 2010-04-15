C+
      SUBROUTINE FIG_FRACAV (INPUT1,INPUT2,NELM,FRAC,OUTPUT)
C
C     F I G _ F R A C A V
C
C     Given two arrays, generates a result array whose elements
C     are a weighted average of the input arrays.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) INPUT1    (Double precision array INPUT1(NELM))
C                   The 1st input array.
C     (>) INPUT2    (Double precision array INPUT2(NELM))
C                   The 2nd input array.
C     (>) NELM      (Integer) The number of elements in each array.
C     (>) FRAC      (Real) The weighting fraction.  The output is
C                   calculated as OUTPUT=INPUT1+(INPUT2-INPUT1)*FRAC,
C                   so FRAC=.5 gives an average, FRAC=1. gives
C                   OUTPUT=INPUT2, FRAC=0. gives OUTPUT=INPUT1.
C     (<) OUTPUT    (Double precision array OUTPUT(NELM)) The output
C                   array.
C
C     Note that any of the arrays may be the same.
C
C     Subroutines / functions used - None
C
C                                            KS / CIT 1st July 1983
C     Modified:
C
C     12th Sept 1985.  KS / AAO. Now works in double precision.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL FRAC
      DOUBLE PRECISION INPUT1(NELM),INPUT2(NELM),OUTPUT(NELM)
C
C     Local variable
C
      INTEGER I
C
      DO I=1,NELM
         OUTPUT(I)=INPUT1(I)+(INPUT2(I)-INPUT1(I))*FRAC
      END DO
C
      END
