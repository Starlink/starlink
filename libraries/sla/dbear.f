      DOUBLE PRECISION FUNCTION sla_DBEAR (A1, B1, A2, B2)
*+
*     - - - - - -
*      D B E A R
*     - - - - - -
*
*  Bearing (position angle) of one point on a sphere relative to another
*  (double precision)
*
*  Given:
*     A1,B1    d    spherical coordinates of one point
*     A2,B2    d    spherical coordinates of the other point
*
*  (The spherical coordinates are RA,Dec, Long,Lat etc, in radians.)
*
*  The result is the bearing (position angle), in radians, of point
*  A2,B2 as seen from point A1,B1.  It is in the range +/- pi.  If
*  A2,B2 is due east of A1,B1 the bearing is +pi/2.  Zero is returned
*  if the two points are coincident.
*
*  P.T.Wallace   Starlink   23 March 1991
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION A1,B1,A2,B2

      DOUBLE PRECISION DA,X,Y


      DA=A2-A1
      Y=SIN(DA)*COS(B2)
      X=SIN(B2)*COS(B1)-COS(B2)*SIN(B1)*COS(DA)
      IF (X.NE.0D0.OR.Y.NE.0D0) THEN
         sla_DBEAR=ATAN2(Y,X)
      ELSE
         sla_DBEAR=0D0
      END IF

      END
