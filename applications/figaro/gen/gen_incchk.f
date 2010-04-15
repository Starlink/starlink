C+
      LOGICAL FUNCTION GEN_INCCHK (ARRAY,N)
C
C     G E N _ I N C C H K
C
C     Determines whether the elements of an array are in
C     ascending order.  (The order does not have to be strictly
C     ascending - adjacent elements may have the same value.)
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY    (Real array ARRAY(N)) The array to be tested.
C     (>) N        (Integer) The number of elements in the array.
C
C     Returns -    (as the function value)
C
C     (<) GEN_INCCHK (Logical) True if the elements are in
C                    increasing order, false otherwise.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                         KS / AAO 19th June 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N
      REAL ARRAY(N)
C
C     Local variables
C
      INTEGER I
C
C     Test the array values
C
      GEN_INCCHK=.TRUE.
      DO I=2,N
         IF (ARRAY(I).LT.ARRAY(I-1)) THEN
            GEN_INCCHK=.FALSE.
            GO TO 300
         END IF
      END DO
  300 CONTINUE
C
      END
