C+
      SUBROUTINE FIG_NPSPIK (NX,DATA,ENDS,NP)
C
C     F I G _ N P S P I K
C
C     Counts the number of points in a spiketrum, including any end
C     points.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NX     (Integer) The number of elements in the spiketrum
C     (>) DATA   (Real array DATA(NX)) The spiketrum data
C     (>) ENDS   (Real array ENDS(4)) In case there are values known
C                to the left and right of the X values given in XDATA,
C                these may be specified in ENDS.  ENDS(1) gives an X
C                value that would precede XDATA(1), ENDS(3) an X value
C                that would follow XDATA(NX), and ENDS(2) and (4) are
C                the corresponding data values.  If such values are
C                not available, the ENDS(1) and/or ENDS(3) should be
C                set to zero.
C     (<) NP     (Integer) The number of values (ie non-zero elements)
C                in the spiketrum, including any end points.
C
C     Common variables used -  None
C
C     Functions / subroutines used -  None
C
C                                      KS / CIT 22nd May 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NP
      REAL    DATA(NX), ENDS(3)
C
C     Local variables
C
      INTEGER IX
C
      NP=0
      DO IX=1,NX
         IF (DATA(IX).NE.0.) NP=NP+1
      END DO
      IF (ENDS(1).NE.0.) NP=NP+1
      IF (ENDS(3).NE.0.) NP=NP+1
C
      END
