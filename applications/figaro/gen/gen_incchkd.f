C+
      LOGICAL FUNCTION GEN_INCCHKD (ARRAY,N)
C
C     G E N _ I N C C H K D
C
C     Determines whether the elements of an array are in
C     ascending order.  (The order does not have to be strictly
C     ascending - adjacent elements may have the same value.)
C     This is a double precision version of GEN_INCCHK.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY    (Double precision array ARRAY(N)) The array to
C                  be tested.
C     (>) N        (Integer) The number of elements in the array.
C
C     Returns -    (as the function value)
C
C     (<) GEN_INCCHKD (Logical) True if the elements are in
C                     increasing order, false otherwise.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                         KS / AAO 4th June 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N
      DOUBLE PRECISION ARRAY(N)
C
C     Local variables
C
      INTEGER I
C
C     Test the array values
C
      GEN_INCCHKD=.TRUE.
      DO I=2,N
         IF (ARRAY(I).LT.ARRAY(I-1)) THEN
            GEN_INCCHKD=.FALSE.
            GO TO 300
         END IF
      END DO
  300 CONTINUE
C
      END
