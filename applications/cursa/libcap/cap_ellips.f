      SUBROUTINE CAP_ELLIPS (A, B, POSANG, PTS, X, Y, STATUS)
*+
*  Name:
*     CAP_ELLIPS
*  Purpose:
*     Calculate a series of points defining an ellipse.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_ELLIPS (A, B, POSANG, PTS; X, Y; STATUS)
*  Description:
*     Calculate a series of points defining an ellipse.  The semi-major
*     axis of the returned ellipse is unity.
*  Arguments:
*     A  =  REAL (Given)
*        Major or semi-major axis (arbitrary units).
*     B  =  REAL (Given)
*        Minor or semi-minor axis (arbitrary units).
*     POSANG  =  REAL (Given)
*        Position angle (degrees)
*     PTS  =  INTEGER (Given)
*        Number of points in the perimeter of the circle.
*     X(PTS)  =  REAL (Returned)
*        X coordinates of the points.
*     Y(PTS)  =  REAL (Returned)
*        Y coordinates of the points.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Compute the scaled minor axis.
*     Compute the rotation angle from the position angle.
*     Compute the angle between points.
*     For each point
*       Compute its angle.
*       Compute its coordinates.
*     end for
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/4/01 (ACD): Original version (from CAP_CIRCL and HAP_ELLIPS).
*     19/4/01 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants.
      INCLUDE 'CAP_PAR'          ! CURSA parametric constants.
*  Arguments Given:
      INTEGER
     :  PTS
      REAL
     :  A,
     :  B,
     :  POSANG
*  Arguments Returned:
      REAL
     :  X(PTS),
     :  Y(PTS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      REAL
     :  BB,      ! Scaled minor axis.
     :  R,       ! Radius of each point.
     :  DEPHI,   ! Angle between successive points.
     :  PHI,     ! Total rotation angle of the current point.
     :  XPOS,    ! X coordinate of the current point.
     :  YPOS,    ! Y     "      "   "     "      "  .
     :  THETA    ! Rotation angle of the major axis above the horizontal
*                  (radians, anticlockwise positive).
      INTEGER
     :  LOOP     ! Loop index.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Compute the scaled minor axis (scaled to a major axis of 1).

         IF (ABS(A) .GT. 1.0E-8  .AND.  ABS(B) .GT. 1.0E-8) THEN
            BB = ABS(B) / ABS(A)
         ELSE
            BB = 1.0E0
         END IF

*
*       Compute the rotation angle from the position angle.

         THETA = - (POSANG - 9.0E1) * CAP__SPI / 1.80E2

*
*       Compute the angle between points.

         DEPHI = (2.0E0 * CAP__SPI) / REAL(PTS - 1)

*
*       Compute the coordinates of each point.

         DO LOOP = 1, PTS
            PHI = DEPHI * REAL(LOOP - 1)

            R = SQRT(
     :        (BB**2) /(  (SIN(PHI)**2) + ((BB*COS(PHI))**2) ) )

            XPOS = R * COS(PHI)
            YPOS = R * SIN(PHI)

            X(LOOP) = (XPOS * COS(THETA)) - (YPOS * SIN(THETA))
            Y(LOOP) = (XPOS * SIN(THETA)) + (YPOS * COS(THETA))
         END DO

      END IF

      END
