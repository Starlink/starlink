C+
      LOGICAL FUNCTION FIG_WCHECK (NX,XDATA1,XDATA2)
C
C     F I G _ W C H E C K
C
C     Checks two arrays of X values against each other, to see
C     if they are 'reasonably' identical.  The criterion for this
C     is that each element should match to 1/10 of a percent of
C     the value in the first array.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NX      (Integer) The number of elements in each array
C     (>) XDATA1  (Real array XDATA1(NX)) The first X array
C     (>) XDATA2  (Real array XDATA2(NX)) The second X array
C
C     Returns -
C
C     (<) FIG_WCHECK  (Logical) True if arrays match, false otherwise
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                        KS / CIT 15th May 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX
      REAL    XDATA1(NX), XDATA2(NX)
C
C     Local variables
C
      INTEGER IX
C
      FIG_WCHECK=.FALSE.
      DO IX=1,NX
         IF (ABS(XDATA1(IX)-XDATA2(IX)).GT.XDATA1(IX)*.001) GO TO 300
      END DO
      FIG_WCHECK=.TRUE.
  300 CONTINUE
C
      END
