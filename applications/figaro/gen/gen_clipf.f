C+
      SUBROUTINE GEN_CLIPF(INPUT,NELM,LOW,HIGH,NLOW,NHIGH,OUTPUT)
C
C     G E N _ C L I P F F
C
C     Clips a floating point array, setting values above a high
C     or below a low threshold to the appropriate threshold value.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) INPUT   (Real array INPUT(NELM)) The input array.  Note that
C                 this can be of any dimension, but is treated as one
C                 dimensional for simplicity and efficiency.
C     (>) NELM    (Integer) Number of elements in INPUT.
C     (>) LOW     (Real) The low threshold
C     (>) HIGH    (Real) The high threshold
C     (<) NLOW    (Integer) The number of elements found below LOW
C     (<) NHIGH   (Integer) The number of elements found above HIGH
C     (<) OUTPUT  (Real array OUTPUT(NELM)) The resulting clipped array.
C                 OUTPUT and INPUT may be the same array.
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C                                           KS / AAO 22nd July 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, NLOW, NHIGH
      REAL    INPUT(NELM), LOW, HIGH, OUTPUT(NELM)
C
C     Local variables
C
      INTEGER I
C
C     Clip the data
C
      NHIGH=0
      NLOW=0
      DO I=1,NELM
         IF (INPUT(I).LT.LOW) THEN
            NLOW=NLOW+1
            OUTPUT(I)=LOW
         ELSE IF (INPUT(I).GT.HIGH) THEN
            NHIGH=NHIGH+1
            OUTPUT(I)=HIGH
         ELSE
            OUTPUT(I)=INPUT(I)
         END IF
      END DO
C
      END
