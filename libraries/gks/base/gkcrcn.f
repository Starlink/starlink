C# IL>=a, OL>=0
      SUBROUTINE GKCRCN (RADIUS,THETA,NMAX,NPTS,DCIRC)
*
* (C) COPYRIGHT ICL & SERC  1988
*

*----------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:         KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE
*  -------
*     Determines the number of points to be used
*     to draw a circular arc in WC and also
*     whether the arc is circular in DC.
*
*  MAINTENANCE LOG
*  ---------------
*     08/02/88  KEVP  Created
*     09/02/88  KEVP  Corrected for full circle
*     15/03/88  KEVP  Removed open segment enquiry etc
*                     (Only needed for hardware segments)
*     15/03/88  KEVP  Add argument to indicate whether arc is
*                     circular in DC
*     29/06/88  KEVP  Prevent possibility of floating point
*                     inaccuracy resulting in negative argument
*                     for square root (S334 and S335).
*     22/07/90  PLP   Commenting brought in line with standard format.
*
*  ARGUMENTS
*  ---------
*     INP RADIUS     The radius of this circle
*     INP THETA      The angle of turn of the arc (-ve if clockwise)
*     INP NMAX       Maximum number of points
*     OUT NPTS       Number of points to be used for arc
*     OUT DCIRC      True, if circular in DC
*
*                    All coords must be World Coords

      REAL RADIUS, THETA
      INTEGER NMAX, NPTS
      LOGICAL DCIRC
*
*  COMMON BLOCK USAGE
*  ------------------
*     None
*
*  LOCALS   all coords in WC unless otherwise stated
*  ------
*   Limits for NPTS
*    (NMAX   Maximim number of points (argument))
*     NMIN   Minimum number of points
*
*   WC to DC Transformation
*     XUN,YUN    array of unit and zero vectors
*     XBT,YBT    XUN,YUN transformed from WC to DC
*     XB1,YB1
*     XB2,YB2    the tranformed basis (axis) vectors of 2D co-ord system
*     B1B1       dot product (XB1,YB1).(XB1.YB1)
*     B1B2       dot product (XB1,YB1).(XB2.YB2)
*     B2B2       dot product (XB2,YB2).(XB2.YB2)
*     BBDET      determinant of matrix BB = (B1B1,B1B2)
*                                           (B1B2,B2B2)
*     BBDIFF     difference of eigen values for matrix BB
*     BBMEAN     mean of eigen values for matrix BB
*     DCMRAD     maximum radius in DC
*
*   Others
*     NSEG       Name of open segment
*     VECRAD     number of vectors per radian (unrounded)
*     VECSN      number of vectors for arc (unrounded)
*     DCCTOL     tolerance for test of DC circularity
*
      INTEGER NMIN
      PARAMETER (NMIN=3)

      REAL XUN(3),YUN(3),  XBT(3),YBT(3)
      REAL XB1,YB1, XB2,YB2, B1B1, B1B2, B2B2
      REAL BBDET, BBDIFF, BBMEAN, DCMRAD
      REAL VECRAD, VECSN, DCCTOL

      PARAMETER (DCCTOL=0.0001)

      DATA  XUN/1.0, 0.0, 0.0/, YUN /0.0, 1.0, 0.0/
*
*  COMMENT
*  -------
*     The input arguments RADIUS and THETA are obtainable
*     from the utility GKCRCE.
*
*     This routine assumes that device coordinates
*     are raster coordinates or are units of resolution.
*
*  ALGORITHM
*  ---------
*     1: The maximum radius of the circle in DC
*     (units of resolution) is found.
*       NB: a circle in WC is not necessarily a circle in DC,
*       but an ellipse.
*
*     2: The number of vectors allocated per radian is
*     set to be the square root of the radius in DC.
*
*     3: The number of vectors allocated to the arc is determined.
*     This is rounded up and one is added for the end point,
*     to give the number of points.
*
*     4-9If this figure is too high or low, it is changed to
*     the nearest number in range.
*
*--------------------------------------------------------------------
*
*     Calculate maximum radius of circle transformed
*     to device coordinates. This is generally an ellipse
*     This is used to determine the number of vectors
*     to be used for the arc.
*
*     Obtain base vectors
      CALL GKTWD (3,XUN,YUN,XBT,YBT)
      XB1 = XBT(1) - XBT(3)
      YB1 = YBT(1) - YBT(3)
      XB2 = XBT(2) - XBT(3)
      YB2 = YBT(2) - YBT(3)
*     then their square magnitudes and inner product
      B1B1 = XB1*XB1 + YB1*YB1
      B2B2 = XB2*XB2 + YB2*YB2
      B1B2 = XB1*XB2 + YB1*YB2

*     and finally the maximum DC radius and circularity
      BBDET  = B1B1*B2B2 - B1B2*B1B2
      BBMEAN = (B1B1 + B2B2)/2.0
      IF(BBDET .LT. BBMEAN*BBMEAN)THEN
         BBDIFF = SQRT(BBMEAN*BBMEAN - BBDET)
      ELSE
*        In case of floating point inaccuracy
         BBDIFF = 0.0
      ENDIF
      DCMRAD = RADIUS*SQRT(BBMEAN + BBDIFF)
      DCIRC  = (BBDIFF/BBMEAN .LT. DCCTOL)
*
*     Determine number of vectors required for arc
      VECRAD = SQRT(DCMRAD)
*     One point is added for rounding up
*     and another for the end of the arc.
      VECSN = ABS(VECRAD*THETA)
      NPTS = NINT(VECSN) + 2
*     and limit it to minimum or maximum number of points
      IF(NPTS .GT. NMAX) NPTS = NMAX
      IF(NPTS .LT. NMIN) NPTS = NMIN

  999 CONTINUE
      RETURN
      END
