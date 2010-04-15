C+
      SUBROUTINE FIG_LSPIKE (NX,XDATA,ENDS,DATA)
C
C     F I G _ L S P I K E
C
C     Converts a spiketrum array (one where all but a few values are
C     zero, these representing sample values of what is assumed to be
C     a smooth continuum) into a spectrum (the continuum represented),
C     by linear interpolation between the points.
C
C     Parameters -  (">" input, "W" workspace, "!" modified, "<" output)
C
C     (>) NX     (Integer) The number of points in the spiketrum.
C     (>) XDATA  (Real array XDATA(NX)) The x-values for the elements
C                of the spiketrum.  The values must be monotonically
C                increasing or decreasing.
C     (>) ENDS   (Real array ENDS(4)) In case there are values known
C                to the left and right of the X values given in XDATA,
C                these may be specified in ENDS.  ENDS(1) gives an X
C                value that would precede XDATA(1), ENDS(3) an X value
C                that would follow XDATA(NX), and ENDS(2) and (4) are
C                the corresponding data values.  If such values are
C                not available, the ENDS(1) and/or ENDS(3) should be
C                set to zero.
C     (!) DATA   (Real array DATA(NX)) Passed as the spiketrum, returned
C                as the interpolated spectrum.
C
C     Common variables used - None
C
C     Functions / subroutines used -  None
C
C                                           KS / CIT 11th July 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX
      REAL    XDATA(NX), ENDS(4), DATA(NX)
C
C     Local variables
C
      LOGICAL MORE
      INTEGER IX, IX1, IXST, IXEN
      REAL    X1, X1WAS, X2, Y1, Y1WAS, Y2
C
C     Find first spike value in data array or in ENDS
C
      MORE=.TRUE.
      IXST=0
      IF (ENDS(1).NE.0.) THEN
         X1=ENDS(1)
         Y1=ENDS(2)
         IX1=0
      ELSE
         DO IX=1,NX
            IF (DATA(IX).NE.0.) THEN
               X1=XDATA(IX)
               Y1=DATA(IX)
               IX1=IX
               GO TO 320
            END IF
         END DO
         MORE=.FALSE.
  320    CONTINUE
      END IF
C
C     Then look for the next value, interpolate, and carry on.
C
      DO WHILE (MORE)
         DO IX=IX1+1,NX
            IF (DATA(IX).NE.0.) THEN
               X2=XDATA(IX)
               Y2=DATA(IX)
               IXEN=IX
               GO TO 340
            END IF
         END DO
         MORE=.FALSE.
         IXEN=NX
         IF (ENDS(3).NE.0.) THEN
            X2=ENDS(3)
            Y2=ENDS(4)
         ELSE
            X1=X1WAS
            Y1=Y1WAS
         END IF
  340    CONTINUE
C
C        Actual interpolation
C
         DO IX=IXST+1,IXEN
            DATA(IX)=Y1+(Y2-Y1)*(XDATA(IX)-X1)/(X2-X1)
         END DO
C
C        Ready to look for next point
C
         X1WAS=X1
         Y1WAS=Y1
         X1=X2
         Y1=Y2
         IXST=IXEN
         IX1=IXEN
      END DO
C
      END
