C+
      SUBROUTINE GEN_SUBAF(NELM,ARRAY1,ARRAY2,ARRAY3)
C
C     G E N _ S U B A F
C
C     Subtracts two floating point arrays.  The arrays
C     may have any dimensions; they are treated here as
C     linear in order to generate more efficient code.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NELM     (Integer) Number of elements in each array
C     (>) ARRAY1   (Real array) Input array
C     (>) ARRAY2   (Real array) Second input array.
C     (<) ARRAY3   (Real array) Result array.  ARRAY3=ARRAY1-ARRAY2
C
C     Note that any of the arrays may be the same.
C
C                                     KS / CIT  18th Feb 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL ARRAY1(NELM),ARRAY2(NELM),ARRAY3(NELM)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY3(I)=ARRAY1(I)-ARRAY2(I)
      END DO
C
      END
