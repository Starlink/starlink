      SUBROUTINE sla_DTP2V (XI, ETA, V0, V)
*+
*     - - - - - -
*      D T P 2 V
*     - - - - - -
*
*  Given the tangent-plane coordinates of a star and the direction
*  cosines of the tangent point, determine the direction cosines
*  of the star.
*
*  (double precision)
*
*  Given:
*     XI,ETA    d       tangent plane coordinates of star
*     V0        d(3)    direction cosines of tangent point
*
*  Returned:
*     V         d(3)    direction cosines of star
*
*  Notes:
*
*  1  If vector V0 is not of unit length, the returned vector V will
*     be wrong.
*
*  2  If vector V0 points at a pole, the returned vector V will be
*     based on the arbitrary assumption that the RA of the tangent
*     point is zero.
*
*  3  This routine is the Cartesian equivalent of the routine sla_DTP2S.
*
*  P.T.Wallace   Starlink   11 February 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION XI,ETA,V0(3),V(3)

      DOUBLE PRECISION X,Y,Z,F,R


      X=V0(1)
      Y=V0(2)
      Z=V0(3)
      F=SQRT(1D0+XI*XI+ETA*ETA)
      R=SQRT(X*X+Y*Y)
      IF (R.EQ.0D0) THEN
         R=1D-20
         X=R
      END IF
      V(1)=(X-(XI*Y+ETA*X*Z)/R)/F
      V(2)=(Y+(XI*X-ETA*Y*Z)/R)/F
      V(3)=(Z+ETA*R)/F

      END
