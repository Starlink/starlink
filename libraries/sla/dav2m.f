      SUBROUTINE sla_DAV2M (AXVEC, RMAT)
*+
*     - - - - - -
*      D A V 2 M
*     - - - - - -
*
*  Form the rotation matrix corresponding to a given axial vector.
*  (double precision)
*
*  A rotation matrix describes a rotation about some arbitrary axis.
*  The axis is called the Euler axis, and the angle through which the
*  reference frame rotates is called the Euler angle.  The axial
*  vector supplied to this routine has the same direction as the
*  Euler axis, and its magnitude is the Euler angle in radians.
*
*  Given:
*    AXVEC  d(3)     axial vector (radians)
*
*  Returned:
*    RMAT   d(3,3)   rotation matrix
*
*  If AXVEC is null, the unit matrix is returned.
*
*  The reference frame rotates clockwise as seen looking along
*  the axial vector from the origin.
*
*  P.T.Wallace   Starlink   June 1989
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION AXVEC(3),RMAT(3,3)

      DOUBLE PRECISION X,Y,Z,PHI,S,C,W



*  Euler angle - magnitude of axial vector - and functions
      X = AXVEC(1)
      Y = AXVEC(2)
      Z = AXVEC(3)
      PHI = SQRT(X*X+Y*Y+Z*Z)
      S = SIN(PHI)
      C = COS(PHI)
      W = 1D0-C

*  Euler axis - direction of axial vector (perhaps null)
      IF (PHI.NE.0D0) THEN
         X = X/PHI
         Y = Y/PHI
         Z = Z/PHI
      END IF

*  Compute the rotation matrix
      RMAT(1,1) = X*X*W+C
      RMAT(1,2) = X*Y*W+Z*S
      RMAT(1,3) = X*Z*W-Y*S
      RMAT(2,1) = X*Y*W-Z*S
      RMAT(2,2) = Y*Y*W+C
      RMAT(2,3) = Y*Z*W+X*S
      RMAT(3,1) = X*Z*W+Y*S
      RMAT(3,2) = Y*Z*W-X*S
      RMAT(3,3) = Z*Z*W+C

      END
