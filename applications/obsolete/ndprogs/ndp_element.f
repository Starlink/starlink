      SUBROUTINE NDP_ELEMENT(DIMS,NDIM,ICOORD,IELEM)
C+
C
C   ---------------------
C   N D P _ E L E M E N T
C   ---------------------
C
C   Description
C   -----------
C   Computes the element number for any pixel coordinate in an image of
C   given dimensions and dimensionality. The element number is counted in
C   the normal FORTRAN sequence from (1,1,...) up to the pixel with the
C   highest possible coordinate in every dimension.
C
C
C   Parameters
C   ----------
C   DIMS    (> integer array). Dimensions of image.
C   NDIM    (> integer). Number of dimensions, maximum 6.
C   ICOORD  (> integer, array). Coordinates of the element.
C   IELEM   (< integer). Element number.
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
C     Parameters
C
      INTEGER DIMS(10),NDIM,ICOORD(6),IELEM
C
C     Local variables
C
      INTEGER I,ISTORE
C
      IELEM=0
      ISTORE=DIMS(1)
C
      DO I=2,NDIM
        IELEM=IELEM+ISTORE*(ICOORD(I)-1)
        ISTORE=ISTORE*DIMS(I)
      END DO
      IELEM=IELEM+ICOORD(1)
C
      END
