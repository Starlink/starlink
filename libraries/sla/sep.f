      REAL FUNCTION sla_SEP (A1, B1, A2, B2)
*+
*     - - - -
*      S E P
*     - - - -
*
*  Angle between two points on a sphere.
*
*  (single precision)
*
*  Given:
*     A1,B1    r     spherical coordinates of one point
*     A2,B2    r     spherical coordinates of the other point
*
*  (The spherical coordinates are [RA,Dec], [Long,Lat] etc, in radians.)
*
*  The result is the angle, in radians, between the two points.  It
*  is always positive.
*
*  Called:  sla_DSEP
*
*  Last revision:   7 May 2000
*
*  Copyright P.T.Wallace.  All rights reserved.
*-

      IMPLICIT NONE

      REAL A1,B1,A2,B2

      DOUBLE PRECISION sla_DSEP



*  Use double precision version.
      sla_SEP = REAL(sla_DSEP(DBLE(A1),DBLE(B1),DBLE(A2),DBLE(B2)))

      END
