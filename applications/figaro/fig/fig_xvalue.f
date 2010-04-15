C+
      REAL FUNCTION FIG_XVALUE(ELEM,XARRAY,NX)
C
C     F I G _ X V A L U E
C
C     Figaro utility, used to interpolate between the values in
C     an axis array.  It is passed a fractional bin number and
C     interpolates between the values in adjacent bins.
C
C     Call:
C
C     VALUE = FIG_XVALUE (ELEM,XARRAY,NX)
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ELEM     (Real,ref) The fractional bin number.
C     (>) XARRAY   (Real array,ref) Array of values corresponding to
C                  the centers of the bins.  That is, the value in
C                  XARRAY(1) corresponds to ELEM=1.0
C     (>) NX       (Integer,ref) The number of elements in XARRAY.
C
C     Returns -
C
C     (<) VALUE    (Real,function value) The interpolated value.
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C                                                 KS / AAO 26th Feb 1988
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX
      REAL ELEM, XARRAY(NX)
C
C     Variables
C
      INTEGER NELM
      REAL X1,Y1,Y2
C
C     All this routine does is a simple linear interpolation,
C     taking care to keep within the range 1..NX
C
      NELM = MIN(NX,INT(ELEM))
      IF (NELM.EQ.NX) THEN
         X1 = FLOAT(NELM - 1)
         Y1 = XARRAY(NELM - 1)
         Y2 = XARRAY(NELM)
      ELSE
         X1 = FLOAT(NELM)
         Y1 = XARRAY(NELM)
         Y2 = XARRAY(NELM + 1)
      END IF
      FIG_XVALUE = Y1 + ((ELEM - X1) * (Y2 - Y1))
C
      END
