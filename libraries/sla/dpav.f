      DOUBLE PRECISION FUNCTION sla_DPAV ( V1, V2 )
*+
*     - - - - -
*      D P A V
*     - - - - -
*
*  Position angle of one celestial direction with respect to another.
*
*  (double precision)
*
*  Given:
*     V1    d(3)    direction cosines of one point
*     V2    d(3)    direction cosines of the other point
*
*  (The coordinate frames correspond to RA,Dec, Long,Lat etc.)
*
*  The result is the bearing (position angle), in radians, of point
*  V2 with respect to point V1.  It is in the range +/- pi.  The
*  sense is such that if V2 is a small distance east of V1, the
*  bearing is about +pi/2.  Zero is returned if the two points
*  are coincident.
*
*  V1 and V2 need not be unit vectors.
*
*  The routine sla_DBEAR performs an equivalent function except
*  that the points are specified in the form of spherical
*  coordinates.
*
*  Patrick Wallace   Starlink   13 July 1997
*
*  Copyright (C) 1997 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION V1(3),V2(3)

      DOUBLE PRECISION X1,Y1,Z1,W,R,XU1,YU1,ZU1,DX,DY,DZ,SQ,CQ



*  Unit vector to point 1
      X1=V1(1)
      Y1=V1(2)
      Z1=V1(3)
      W=SQRT(X1*X1+Y1*Y1+Z1*Z1)
      IF (W.NE.0D0) THEN
         X1=X1/W
         Y1=Y1/W
         Z1=Z1/W
      END IF

*  Unit vector "north" from point 1
      R=SQRT(X1*X1+Y1*Y1)
      IF (R.EQ.0.0) R=1D-5
      W=Z1/R
      XU1=-X1*W
      YU1=-Y1*W
      ZU1=R

*  Vector from point 1 to point 2
      DX=V2(1)-X1
      DY=V2(2)-Y1
      DZ=V2(3)-Z1

*  Position angle
      SQ=DX*YU1*Z1+DY*ZU1*X1+DZ*XU1*Y1-DZ*YU1*X1-DY*XU1*Z1-DX*ZU1*Y1
      CQ=DX*XU1+DY*YU1+DZ*ZU1
      IF (SQ.EQ.0D0.AND.CQ.EQ.0D0) CQ=1D0
      sla_DPAV=ATAN2(SQ,CQ)

      END
