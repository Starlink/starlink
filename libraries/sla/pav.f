      REAL FUNCTION sla_PAV ( V1, V2 )
*+
*     - - - -
*      P A V
*     - - - -
*
*  Position angle of one celestial direction with respect to another.
*
*  (single precision)
*
*  Given:
*     V1    r(3)    direction cosines of one point
*     V2    r(3)    direction cosines of the other point
*
*  (The coordinate frames correspond to RA,Dec, Long,Lat etc.)
*
*  The result is the bearing (position angle), in radians, of point
*  V2 with respect to point V1.  It is in the range +/- pi.  The
*  sense is such that if V2 is a small distance east of V1, the
*  bearing is about +pi/2.  Zero is returned if the two points
*  are coincident.
*
*  V1 and V2 do not have to be unit vectors.
*
*  The routine sla_BEAR performs an equivalent function except
*  that the points are specified in the form of spherical
*  coordinates.
*
*  Called:  sla_DPAV
*
*  Patrick Wallace   Starlink   23 May 1997
*
*  Copyright (C) 1997 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL V1(3),V2(3)

      INTEGER I
      DOUBLE PRECISION D1(3),D2(3)

      DOUBLE PRECISION sla_DPAV


*  Call the double precision version
      DO I=1,3
         D1(I)=V1(I)
         D2(I)=V2(I)
      END DO
      sla_PAV=sla_DPAV(D1,D2)

      END
