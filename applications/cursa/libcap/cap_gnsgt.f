      SUBROUTINE CAP_GNSGT (XPOS, YPOS, RANGE, STATUS)
*+
*  Name:
*     CAP_GNSGT
*  Purpose:
*     Plot an open-centred cross (a 'gunsight') at a given position.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GNSGT (XPOS, YPOS, RANGE; STATUS)
*  Description:
*     Plot an open-centred cross (a 'gunsight') at a given position.
*  Arguments:
*     XPOS  =  REAL (Given)
*        X coordinate of the centre of the cross.
*     YPOS  =  REAL (Given)
*        Y coordinate of the centre of the cross.
*     RANGE  =  REAL (Given)
*        Range of the plot.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each arm of the cross
*       Compute the start and stop positions.
*       Plot the arm.
*     end for
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/8/96 (ACD): Original version.
*     3/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      REAL
     :  XPOS,
     :  YPOS,
     :  RANGE
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Constants:
      REAL START    ! Relative start position of arm.
      PARAMETER (START = 5.0E-2)

      REAL STOP     ! Relative stop position of arm.
      PARAMETER (STOP = 2.5E-2)
*  Local Variables:
      REAL
     :  XCORD(2),   ! X coordinates of the arm.
     :  YCORD(2)    ! Y     "       "   "   " .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       For each arm compute the coordinates of its end points and then
*       plot it.

         XCORD(1) = XPOS
         XCORD(2) = XPOS
         YCORD(1) = YPOS + (RANGE * START)
         YCORD(2) = YPOS + (RANGE * STOP)

         CALL PGLINE (2, XCORD, YCORD)

         XCORD(1) = XPOS + (RANGE * START)
         XCORD(2) = XPOS + (RANGE * STOP)
         YCORD(1) = YPOS
         YCORD(2) = YPOS

         CALL PGLINE (2, XCORD, YCORD)

         XCORD(1) = XPOS
         XCORD(2) = XPOS
         YCORD(1) = YPOS - (RANGE * START)
         YCORD(2) = YPOS - (RANGE * STOP)

         CALL PGLINE (2, XCORD, YCORD)

         XCORD(1) = XPOS - (RANGE * START)
         XCORD(2) = XPOS - (RANGE * STOP)
         YCORD(1) = YPOS
         YCORD(2) = YPOS

         CALL PGLINE (2, XCORD, YCORD)

      END IF

      END
