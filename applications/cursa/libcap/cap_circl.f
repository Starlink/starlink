      SUBROUTINE CAP_CIRCL (PTS, X, Y, STATUS)
*+
*  Name:
*     CAP_CIRCL
*  Purpose:
*     Calculate a series of points defining a circle of unit radius.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CIRCL (PTS; X, Y; STATUS)
*  Description:
*     Calculate a series of points defining a circle of unit radius.
*  Arguments:
*     PTS  =  INTEGER (Given)
*        Number of points in the perimeter of the circle.
*     X(PTS)  =  REAL (Returned)
*        X coordinates of the points.
*     Y(PTS)  =  REAL (Returned)
*        Y coordinates of the points.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
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
*     5/6/97  (ACD): Original version.
*     18/4/01 (ACD): Corrected mistake in prologue.
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
*  Arguments Returned:
      REAL
     :  X(PTS),
     :  Y(PTS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      REAL
     :  DEPHI,   ! Angle between successive points.
     :  PHI      ! Total rotation angle of the current point.
      INTEGER
     :  LOOP     ! Loop index.
*.

      IF (STATUS .EQ. SAI__OK) THEN

         DEPHI = (2.0E0 * CAP__SPI) / REAL(PTS - 1)

         DO LOOP = 1, PTS
            PHI = DEPHI * REAL(LOOP - 1)

            X(LOOP) = COS(PHI)
            Y(LOOP) = SIN(PHI)
         END DO

      END IF

      END
