      SUBROUTINE sla_CC2S (V, A, B)
*+
*     - - - - -
*      C C 2 S
*     - - - - -
*
*  Direction cosines to spherical coordinates (single precision)
*
*  Given:
*     V     r(3)   x,y,z vector
*
*  Returned:
*     A,B   r      spherical coordinates in radians
*
*  The spherical coordinates are longitude (+ve anticlockwise
*  looking from the +ve latitude pole) and latitude.  The
*  Cartesian coordinates are right handed, with the x axis
*  at zero longitude and latitude, and the z axis at the
*  +ve latitude pole.
*
*  If V is null, zero A and B are returned.
*  At either pole, zero A is returned.
*
*  P.T.Wallace   Starlink   July 1989
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL V(3),A,B

      REAL X,Y,Z,R


      X = V(1)
      Y = V(2)
      Z = V(3)
      R = SQRT(X*X+Y*Y)

      IF (R.EQ.0.0) THEN
         A = 0.0
      ELSE
         A = ATAN2(Y,X)
      END IF

      IF (Z.EQ.0.0) THEN
         B = 0.0
      ELSE
         B = ATAN2(Z,R)
      END IF

      END
