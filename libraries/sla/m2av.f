      SUBROUTINE sla_M2AV (RMAT, AXVEC)
*+
*     - - - - -
*      M 2 A V
*     - - - - -
*
*  From a rotation matrix, determine the corresponding axial vector
*  (single precision)
*
*  A rotation matrix describes a rotation about some arbitrary axis.
*  The axis is called the Euler axis, and the angle through which the
*  reference frame rotates is called the Euler angle.  The axial
*  vector returned by this routine has the same direction as the
*  Euler axis, and its magnitude is the Euler angle in radians.  (The
*  magnitude and direction can be separated by means of the routine
*  sla_VN.)
*
*  Given:
*    RMAT   r(3,3)   rotation matrix
*
*  Returned:
*    AXVEC  r(3)     axial vector (radians)
*
*  The reference frame rotates clockwise as seen looking along
*  the axial vector from the origin.
*
*  If RMAT is null, so is the result.
*
*  P.T.Wallace   Starlink   11 April 1990
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL RMAT(3,3),AXVEC(3)

      REAL X,Y,Z,S2,C2,PHI,F



      X = RMAT(2,3)-RMAT(3,2)
      Y = RMAT(3,1)-RMAT(1,3)
      Z = RMAT(1,2)-RMAT(2,1)
      S2 = SQRT(X*X+Y*Y+Z*Z)
      IF (S2.NE.0.0) THEN
         C2 = (RMAT(1,1)+RMAT(2,2)+RMAT(3,3)-1.0)
         PHI = ATAN2(S2/2.0,C2/2.0)
         F = PHI/S2
         AXVEC(1) = X*F
         AXVEC(2) = Y*F
         AXVEC(3) = Z*F
      ELSE
         AXVEC(1) = 0.0
         AXVEC(2) = 0.0
         AXVEC(3) = 0.0
      END IF

      END
