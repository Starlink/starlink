C+
      SUBROUTINE GEN_FRANGEF(ARRAY,FBAD,IST,IEN,VMAX,VMIN)
C
C     G E N _ F R A N G E F
C
C     Finds the maximum and minimum values in an array,
C     ignoring any values that have been flagged as `bad'.
C     If all values in the array are bad, both max and min are
C     set to zero.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY     (Real array ARRAY(IEN) - or more) Array
C                   containing the values to be checked.
C     (>) FBAD      (Real) The flag value used to indicate that an
C                   element is to be ignored.
C     (>) IST       (Integer) The first element of ARRAY to be
C                   examined.
C     (>) IEN       (Integer) The last element of ARRAY to be
C                   examined.
C     (<) VMAX      (Real) The maximum value of those examined.
C     (<) VMIN      (Real) The minimum value of those examined.
C
C                                      KS / AAO 6th Feb 1989
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IST,IEN
      REAL ARRAY(IEN),FBAD,VMIN,VMAX
C
C     Local variables
C
      INTEGER I, ISTART
C
      ISTART=0
      DO I=IST,IEN
         IF (ARRAY(I).NE.FBAD) THEN
            ISTART=I
            GO TO 320   ! Break out of I loop
         END IF
      END DO
  320 CONTINUE
      IF (ISTART.EQ.0) THEN
         VMIN=0.0
         VMAX=VMIN
      ELSE
         VMIN=ARRAY(ISTART)
         VMAX=VMIN
         DO I=ISTART+1,IEN
            IF (ARRAY(I).NE.FBAD) THEN
               VMIN=MIN(VMIN,ARRAY(I))
               VMAX=MAX(VMAX,ARRAY(I))
            END IF
         END DO
      END IF
C
      END
