      SUBROUTINE sla_TP2V (XI, ETA, V0, V)
*+
*     - - - - -
*      T P 2 V
*     - - - - -
*
*  Given the tangent-plane coordinates of a star and the direction
*  cosines of the tangent point, determine the direction cosines
*  of the star.
*
*  (single precision)
*
*  Given:
*     XI,ETA    r       tangent plane coordinates of star
*     V0        r(3)    direction cosines of tangent point
*
*  Returned:
*     V         r(3)    direction cosines of star
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
*  3  This routine is the Cartesian equivalent of the routine sla_TP2S.
*
*  P.T.Wallace   Starlink   11 February 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL XI,ETA,V0(3),V(3)

      REAL X,Y,Z,F,R


      X=V0(1)
      Y=V0(2)
      Z=V0(3)
      F=SQRT(1.0+XI*XI+ETA*ETA)
      R=SQRT(X*X+Y*Y)
      IF (R.EQ.0.0) THEN
         R=1E-20
         X=R
      END IF
      V(1)=(X-(XI*Y+ETA*X*Z)/R)/F
      V(2)=(Y+(XI*X-ETA*Y*Z)/R)/F
      V(3)=(Z+ETA*R)/F

      END
