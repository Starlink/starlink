C+
      SUBROUTINE GEN_ADDAD(NELM,ARRAY1,ARRAY2,ARRAY3)
C
C     G E N _ A D D A D
C
C     Adds together two double precision arrays.  The arrays
C     may have any dimensions; they are treated here as
C     linear in order to generate more efficient code.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NELM     (Integer) Number of elements in each array
C     (>) ARRAY1   (Double precision array) Input array
C     (>) ARRAY2   (Double precision array) Second input array.
C     (<) ARRAY3   (Double precision array) Result array.
C                  ARRAY3=ARRAY1+ARRAY2
C
C     Note that any of the arrays may be the same.
C
C                                     KS / AAO 10th Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      DOUBLE PRECISION ARRAY1(NELM),ARRAY2(NELM),ARRAY3(NELM)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY3(I)=ARRAY2(I)+ARRAY1(I)
      END DO
C
      END
