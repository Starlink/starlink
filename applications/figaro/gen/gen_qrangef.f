C+
      SUBROUTINE GEN_QRANGEF(ARRAY,QARRAY,IST,IEN,VMAX,VMIN)
C
C     G E N _ Q R A N G E F
C
C     Finds the maximum and minimum values in an array, ignoring
C     values whose corresponding elements in a data quality array
C     are non-zero.  If all elements of the array are flagged as
C     being of bad quality, then the max and min are set to zero.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY     (Real array ARRAY(IEN) - or more) Array
C                   containing the values to be checked.
C     (>) QARRAY    (Byte array QARRAY(IEN) - or more) Data quality
C                   array corresponding to ARRAY.
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
      BYTE QARRAY(IEN)
      REAL ARRAY(IEN),VMIN,VMAX
C
C     Local variables
C
      INTEGER I,ISTART
C
      ISTART=0
      DO I=IST,IEN
         IF (QARRAY(I).EQ.0) THEN
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
            IF (QARRAY(I).EQ.0) THEN
               VMIN=MIN(VMIN,ARRAY(I))
               VMAX=MAX(VMAX,ARRAY(I))
            END IF
         END DO
      END IF
C
      END
