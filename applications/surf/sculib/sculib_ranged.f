C+
      SUBROUTINE SCULIB_RANGED(ARRAY,IST,IEN,VMAX,VMIN)
C
C     S C U L I B _ R A N G E D
C
C     Finds the maximum and minimum values in a double precision array.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY     (Double precision array ARRAY(IEN) - or more) Array
C                   containing the values to be checked.
C     (>) IST       (Integer) The first element of ARRAY to be
C                   examined.
C     (>) IEN       (Integer) The last element of ARRAY to be
C                   examined.
C     (<) VMAX      (Double precision) The maximum value of those examined.
C     (<) VMIN      (Double precision) The minimum value of those examined.
C
C                                      JFL / ROE 20 Sept 1991 - adapted from
C                                      GEN_RANGEF by Keith Shortridge
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IST,IEN
      DOUBLE PRECISION ARRAY(IEN),VMIN,VMAX
C
C     Local variables
C
      INTEGER I
C
      VMIN=ARRAY(IST)
      VMAX=VMIN
      IF (IST.LT.IEN) THEN
         DO I=IST+1,IEN
            VMIN=MIN(VMIN,ARRAY(I))
            VMAX=MAX(VMAX,ARRAY(I))
         END DO
      END IF
C
      END
