C+
      INTEGER FUNCTION GEN_PMAX (ARRAY,NELM)
C
C     G E N _ P M A X
C
C     Returns the number of the pixel containing the maximum
C     value in an array.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY    (Real array ARRAY(NELM)) The array in question.
C     (>) NELM     (Integer) The number of elements in ARRAY.
C
C     Returns -
C
C     (<) GEN_PMAX (Integer) The number of the element containing the
C                  highest value in the array.  If there are two or
C                  more elements with the same highest value, the
C                  lowest element number is returned.
C
C     Common variables use - None
C
C     Subroutines / functions used - None
C
C                                             KS / CIT 3rd Feb 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL ARRAY(NELM)
C
C     Local variables
C
      INTEGER I
      REAL VMAX
C
C     Find the maximum element
C
      VMAX=ARRAY(1)
      GEN_PMAX=1
      DO I=1,NELM
         IF (ARRAY(I).GT.VMAX) THEN
            VMAX=ARRAY(I)
            GEN_PMAX=I
         END IF
      END DO
C
      END
