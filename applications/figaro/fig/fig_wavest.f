C+
      REAL FUNCTION FIG_WAVEST (IX,NX,XDATA)
C
C     F I G _ W A V E S T
C
C     Figaro holds wavelength data in an X-array where each element
C     holds the value corresponding to the CENTER of the element.
C     This routine calculates - by interpolation - the value that
C     corresponds to the START of the element.  It will also calculate
C     values that lie outside the actual range of the data, so can be
C     used to calculate the value corresponding to the END of the final
C     element by calling it with IX set equal to NX+1, for example.
C
C     Parameters  -   (">" input, "<" output)
C
C     (>) IX     (Integer) The element number
C     (>) NX     (Integer) The number of elements in the array
C     (>) XDATA  (Real array XDATA(NX)) The data array
C
C     Returns
C
C     (<) FIG_WAVEST  (Real) The interpolated start value
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                            KS / CIT 14th May 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IX, NX
      REAL    XDATA(NX)
C
      IF (IX.LE.1) THEN
         FIG_WAVEST=XDATA(1)-(FLOAT(1-IX)+.5)*(XDATA(2)-XDATA(1))
      ELSE IF (IX.GT.NX) THEN
         FIG_WAVEST=XDATA(NX)+(FLOAT(IX-NX)-.5)*
     :                                (XDATA(NX)-XDATA(NX-1))
      ELSE
         FIG_WAVEST=(XDATA(IX)+XDATA(IX-1))*.5
      END IF
C
      END
