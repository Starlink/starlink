C+
      SUBROUTINE GEN_RANGEF(ARRAY,IST,IEN,VMAX,VMIN)
C
C     G E N _ R A N G E F
C
C     Finds the maximum and minimum values in an array.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY     (Real array ARRAY(IEN) - or more) Array
C                   containing the values to be checked.
C     (>) IST       (Integer) The first element of ARRAY to be
C                   examined.
C     (>) IEN       (Integer) The last element of ARRAY to be
C                   examined.
C     (<) VMAX      (Real) The maximum value of those examined.
C     (<) VMIN      (Real) The minimum value of those examined.
C
C                                      KS / CIT  2nd Jan 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IST,IEN
      REAL ARRAY(IEN),VMIN,VMAX
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
