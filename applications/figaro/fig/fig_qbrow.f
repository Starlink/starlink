C+
      LOGICAL FUNCTION FIG_QBROW (ARRAY,NY,IY,FACTOR)
C
C     F I G _ Q B R O W
C
C     Given an array which is assumed to be a summed cut through
C     a CCD image, attempts to determine whether or not a given
C     pixel may represent a bad row.  See FIG_ABROWS for a
C     description of the criteria used.
C
C     Parameters - (">" input, "<" output)
C
C     (>) ARRAY    (Real array ARRAY(NY)) The cut data array
C     (>) NY       (Integer) The number of pixels in the cut.
C     (>) IY       (Integer) The pixel to be tested.  IY should be
C                  in the range 2..NY-1
C     (>) FACTOR   (Real) The factor by which the median must be
C                  exceeded.
C
C     Returns -
C
C     FIG_QBROW    (Logical) True if the pixel represents a possible
C                  bad row, false otherwise.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     GEN_QFMED    (GEN_ package) Determine the median of an array.
C
C                                                KS / CIT 24th Feb 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NY,IY
      REAL ARRAY(NY),FACTOR
C
C     Function used
C
      REAL GEN_QFMED
C
C     Number of pixels to cover either side of test point
C
      INTEGER NPIX
      PARAMETER (NPIX=20)
C
C     Local variables
C
      INTEGER I,I1,I2,NVAL
      REAL DIFF1,DIFF2,DIFFS(NPIX+NPIX+1),VMED
C
C     Test the pixel
C
      DIFF1=ARRAY(IY+1)-ARRAY(IY)
      DIFF2=ARRAY(IY-1)-ARRAY(IY)
      IF ((DIFF2.GT.0.).AND.(DIFF1.GT.0.)) THEN
         I1=MAX(1,IY-NPIX)
         I2=MIN(NY-1,I1+NPIX+NPIX)
         NVAL=0
         DO I=I1,I2
            NVAL=NVAL+1
            DIFFS(NVAL)=ABS(ARRAY(I)-ARRAY(I+1))
         END DO
         VMED=GEN_QFMED(DIFFS,NVAL)*FACTOR
         FIG_QBROW=(DIFF1.GT.VMED).AND.(DIFF2.GT.VMED)
      ELSE
         FIG_QBROW=.FALSE.
      END IF
C
      END
