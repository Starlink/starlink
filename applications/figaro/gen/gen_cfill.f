C+
      SUBROUTINE GEN_CFILL (I1,I2,VALUE,DATA)
C
C     G E N _ C F I L L
C
C     Sets a specified range of elements in a real array
C     to a constant value.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) I1      (Integer) First element of the array to be set
C     (>) I2      (Integer) Last element of the array to be set
C     (>) VALUE   (Real) Value to which elements are to be set
C     (<) DATA    (Real array DATA(1..>=I2)) Array in question.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                        KS / AAO 27th March 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER I1,I2
      REAL VALUE, DATA(I2)
C
C     Local variables
C
      INTEGER I
C
C     Set region
C
      DO I=I1,I2
         DATA(I)=VALUE
      END DO
C
      END
