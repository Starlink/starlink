C+
      SUBROUTINE FIG_2ND_LOWEST (result,d1,d2,d3,d4,d5,d6,d7,d8)
C
C     F I G _ 2 N D _ L O W E S T
C
C     Finds the second lowest value of eight parameters.
C
C     Parameters -  (">" input, "<" output)
C
C     (<) RESULT  (Real) The second lowest of D1 thru D8
C     (>) D1      (Real) The first of the eight values to be compared
C     (>) D8      (Real) The last of the eight values to be compared
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C                                          MCBA / AAO 29-Aug-1988
C     Modified:
C
C     6th  Oct 1992  HME / UoE, Starlink.  Uppercase code, no length
C                    specifications in declarations (*4).
C+
C
C This routine was written in a hurry, and probably is not the ideal way
C of proceeding. Please forward your suggestions.
C
      IMPLICIT NONE
C
C     Parameters
C
      REAL RESULT,D1,D2,D3,D4,D5,D6,D7,D8
C
C     Local variables
C
      REAL D(8),DM,DMIN
      INTEGER I,J
C
C For convenience, load the eight value to be compared into an array.
C
      D(1) = D1
      D(2) = D2
      D(3) = D3
      D(4) = D4
      D(5) = D5
      D(6) = D6
      D(7) = D7
      D(8) = D8
C
C Find the smallest value.
C
      DMIN = D1
      J = 1
      DO I=2,8
        IF (D(I).LT.DMIN) THEN
          DMIN = D(I)
          J = I
        END IF
      END DO
C
C Now find the second smallest by looking for the value which is closest to
C the smallest one. First, we chose a suitable initial value.
C
      IF (J.EQ.1) THEN
        DM = D2 - DMIN
        RESULT = D2
      ELSE
        DM = D1 - DMIN
        RESULT = D1
      END IF
C
C Now loop through, and look for a closer value.
C
      DO I=2,8
        IF (((D(I)-DMIN).LT.DM).AND.(I.NE.J)) THEN
          DM = D(I)-DMIN
          RESULT = D(I)
        END IF
      END DO

      END
