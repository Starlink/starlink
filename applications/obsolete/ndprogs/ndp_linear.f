      SUBROUTINE NDP_LINEAR
     &  (NDIM,NPIX,PIXELS,VALUES,CPIXEL,RESULT)
C+
C
C   -------------------
C   N D P _ L I N E A R
C   -------------------
C
C   Description
C   -----------
C   Interpolates a data value at any point in a rectangular grid cell in
C   any number of dimensions. The supplied points must be the grid cell
C   corners.
C
C
C   Parameters
C   ----------
C   NDIM    (> integer). Number of dimensions in grid cell.
C   NPIX    (> integer). Number of grid cell corner pixels.
C   PIXELS  (> integer array). Coordinates of grid cell corners. Must be in
C           X,Y,Z... order and values must be 0 or 1.
C   VALUES  (> real array). Data values at corners, in X,Y,Z... order.
C   CPIXEL  (> real array). Coordinates of point to be interpolated. Must
C           be in the range 0.0 to 1.0.
C   RESULT  (< real). Interpolated data value.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   None.
C
C
C   INCLUDE statements
C   ------------------
C   None.
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Chris Benn RGO (RGVAD::CRB or CRB@UK.AC.RGO.STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C   Parameters
C
      INTEGER PIXELS(6,729)
      REAL    VALUES(64)
      REAL    CPIXEL(6)
      INTEGER NDIM
      INTEGER NPIX
      REAL    RESULT
C
C   Local variables
C
      INTEGER I,J
      REAL    WORK1,WORK2,WORK3,PRODUCT
C
C   Interpolate
C
      RESULT=0.0
      DO J=1,NPIX
        PRODUCT=1.0
        DO I=1,NDIM
          WORK1=1.0-REAL(PIXELS(I,J))
          WORK2=2.0*REAL(PIXELS(I,J))-1.0
          WORK3=WORK1+WORK2*CPIXEL(I)
          PRODUCT=PRODUCT*WORK3
        END DO
        RESULT=RESULT+VALUES(J)*PRODUCT
      END DO
C
      END
