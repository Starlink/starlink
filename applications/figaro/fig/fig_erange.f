C+
      SUBROUTINE FIG_ERANGE(ZVALS,ERRORS,IXST,IXEN,VMAX,VMIN)
C
C     F I G _ E R A N G E
C
C     Calculates the limits of the error bar values for an array of
C     data with error values.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) ZVALS    (Real array ZVALS(IXEN)) The data values
C     (>) ERRORS   (Real array ERRORS(IXEN)) The error values
C     (>) IXST     (Integer) The first array element to be used
C     (>) IXEN     (Integer) The last array element to be used
C     (<) VMAX     (Real) The maximum value of data+error
C     (<) VMIN     (Real) The minimum value of data-errro
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                            KS / AAO 14th June 1985
C     Modified:
C
C     14th July 1986  KS / AAO. Somewhat belated change to treat errors
C                     as percentage errors rather than absolute.
C     22nd July 1986  KS / AAO. Almost immediately, reverts to use of
C                     absolute error values!
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IXST, IXEN
      REAL    ZVALS(IXEN), ERRORS(IXEN), VMAX, VMIN
C
C     Local variables
C
      INTEGER I
C
C     Find max and min
C
      VMAX=ZVALS(IXST)+ERRORS(IXST)
      VMIN=ZVALS(IXST)-ERRORS(IXST)
C
      DO I=IXST,IXEN
         IF ((ZVALS(I)+ERRORS(I)).GT.VMAX) VMAX=ZVALS(I)+ERRORS(I)
         IF ((ZVALS(I)-ERRORS(I)).LT.VMIN) VMIN=ZVALS(I)-ERRORS(I)
      END DO
C
      END
