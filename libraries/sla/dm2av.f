      SUBROUTINE sla_DM2AV (RMAT, AXVEC)
*+
*     - - - - - -
*      D M 2 A V
*     - - - - - -
*
*  From a rotation matrix, determine the corresponding axial vector.
*  (double precision)
*
*  A rotation matrix describes a rotation about some arbitrary axis.
*  The axis is called the Euler axis, and the angle through which the
*  reference frame rotates is called the Euler angle.  The axial
*  vector returned by this routine has the same direction as the
*  Euler axis, and its magnitude is the Euler angle in radians.  (The
*  magnitude and direction can be separated by means of the routine
*  sla_DVN.)
*
*  Given:
*    RMAT   d(3,3)   rotation matrix
*
*  Returned:
*    AXVEC  d(3)     axial vector (radians)
*
*  The reference frame rotates clockwise as seen looking along
*  the axial vector from the origin.
*
*  If RMAT is null, so is the result.
*
*  P.T.Wallace   Starlink   19 April 2000
*
*  Copyright (C) 2000 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION RMAT(3,3),AXVEC(3)

      DOUBLE PRECISION X,Y,Z,S2,C2,PHI,F



      X = RMAT(2,3)-RMAT(3,2)
      Y = RMAT(3,1)-RMAT(1,3)
      Z = RMAT(1,2)-RMAT(2,1)
      S2 = SQRT(X*X+Y*Y+Z*Z)
      IF (S2.NE.0D0) THEN
         C2 = RMAT(1,1)+RMAT(2,2)+RMAT(3,3)-1D0
         PHI = ATAN2(S2,C2)
         F = PHI/S2
         AXVEC(1) = X*F
         AXVEC(2) = Y*F
         AXVEC(3) = Z*F
      ELSE
         AXVEC(1) = 0D0
         AXVEC(2) = 0D0
         AXVEC(3) = 0D0
      END IF

      END
